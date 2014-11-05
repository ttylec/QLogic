module QLogic.BoxProduct where

import QLogic
import Data.List
import Data.Maybe
import Control.Monad

-- |Simple four-atom lattice
data Lattice4 = Zero | X1 | X0 | Y1 | Y0 | One deriving (Bounded, Enum, Eq, Ord, Show)

instance Finite Lattice4 where
        elements = [minBound..]

instance Poset Lattice4 where
        Zero .<. _ = True

        X1 .<. One = True
        X1 .<. X1 = True

        X0 .<. One = True 
        X0 .<. X0 = True

        Y1 .<. One = True
        Y1 .<. Y1 = True

        Y0 .<. One = True
        Y0 .<. Y0 = True

        One .<. One = True
        _ .<. _ = False

instance Logic Lattice4 where
        one = One
        zero = Zero
        ortho Zero = One
        ortho X1 = X0
        ortho X0 = X1
        ortho Y1 = Y0
        ortho Y0 = Y1
        ortho One = Zero

instance AtomicLogic Lattice4 where
        atoms = [X1, X0, Y1, Y0]

data FreeProduct a b = FreeProd a b | FreePlus (FreeProduct a b) (FreeProduct a b) --deriving (Show)

instance (Show a, Show b) => Show (FreeProduct a b) where
        show (FreeProd a b) = "(" ++ (show a) ++ (show b) ++ ")"
        show (FreePlus a p) = (show a) ++ "⊕" ++ (show p)

instance (Poset a, Poset b) => Eq (FreeProduct a b) where
        a == b = a .<. b && b .<. a

instance (Poset a, Poset b) => Poset (FreeProduct a b) where
        (FreeProd a1 a2) .<. (FreeProd b1 b2) = a1 .<. b1 && a2 .<. b2
        a .<. b = all (\x -> any (x .<.) blist) alist
            where
                alist = freeToList a
                blist = freeToList b

(<>) :: (Logic a, Logic b) => a -> b -> FreeProduct a b
(<>) a b
    | (a == zero) || (b == zero) = FreeProd zero zero
    | otherwise = FreeProd a b

(<+>) :: (Logic a, Logic b) => FreeProduct a b -> FreeProduct a b -> FreeProduct a b
(<+>) a@(FreeProd _ _) b = FreePlus a b
(<+>) a@(FreePlus _ _) b@(FreeProd _ _) = FreePlus b a
(<+>) a@(FreePlus a1 a2) b@(FreePlus _ _) = a1 <+> (a2 <+> b)

freeToList :: FreeProduct a b -> [FreeProduct a b]
freeToList a@(FreeProd _ _) = [a]
freeToList (FreePlus a b) = a:(freeToList b)

freeFromList :: (Logic a, Logic b) => [FreeProduct a b] -> FreeProduct a b
freeFromList alist = foldl1 (<+>) alist

-- newtype EquivList a = EquivList [a] deriving (Show)
-- 
-- eqlFromList = EquivList . nub 
-- 
-- -- data BoxProduct a b = BoxProduct (EquivList (FreeProduct a b)) deriving (Eq, Show)

data BoxProduct a b = BoxProduct [FreeProduct a b] deriving (Show)

instance (Logic a, Logic b) => Eq (BoxProduct a b) where
        (BoxProduct []) == (BoxProduct []) = True
        (BoxProduct as) == (BoxProduct bs) = any (`elem` bs) as

instance (Logic a, Logic b) => Poset (BoxProduct a b) where
        (BoxProduct as) .<. (BoxProduct bs) = any id [a .<. b | a <- as, b <- bs]

boxFromList :: (Logic a, Logic b) => [FreeProduct a b] -> BoxProduct a b
boxFromList = BoxProduct . nub

boxToList :: (Logic a, Logic b) => BoxProduct a b -> [FreeProduct a b]
boxToList (BoxProduct as) = as

reduce :: (Logic a, Logic b) => FreeProduct a b -> Maybe (FreeProduct a b)
reduce a@(FreeProd _ _) = Just a
reduce (FreePlus a@(FreeProd a1 a2) b@(FreeProd b1 b2))
    | a .<. b = Just b
    | b .<. a = Just a
    | a1 == b1 = if a2 -|- b2 then (Just $ a1 <> (a2 \./ b2)) else Nothing
    | a2 == b2 = if a1 -|- b1 then (Just $ (a1 \./ b1) <> a2) else Nothing
    | (a1 /= b1) && (a2 `isOrthogonal` b2) = Just $ a <+> b
    | (a2 /= b2) && (a1 `isOrthogonal` b1) = Just $ a <+> b
    | otherwise = Nothing
reduce (FreePlus a@(FreeProd _ _) (FreePlus b c)) = reduce (a <+> b) >>= (`reduce'` c)
    where
        reduce' a@(FreeProd _ _) p = Just $ a <+> p
        reduce' (FreePlus a b) p = liftM (b <+>) $ reduce $ a <+> p

reduceAll :: (Logic a, Logic b) => FreeProduct a b -> Maybe (FreeProduct a b)
reduceAll a = firstCycleBy (lifted exprEq) (reduce =<<) $ Just a
    where
        lifted f (Just a) (Just b) = f a b
        lifted _ Nothing Nothing = True
        lifted _ _ _ = False

exprEq a@(FreeProd a1 a2) b@(FreeProd b1 b2) = a1 == b1 && a2 == b2
exprEq (FreePlus a b) (FreePlus c d) = a == c && b == d
exprEq _ _ = False

fromFreeProduct :: (Logic a, Logic b) => FreeProduct a b -> BoxProduct a b
fromFreeProduct = boxFromList . fromFreeProduct' 
        
fromFreeProduct' :: (Logic a, Logic b) => FreeProduct a b -> [FreeProduct a b]
fromFreeProduct' a 
    | any (==Nothing) reductions = []
    | otherwise = nub $ a:(catMaybes reductions)
    where
        reductions = map (reduceAll . freeFromList) $ permutations $ freeToList a

reduceBoxProductList :: (Logic a, Logic b) => [BoxProduct a b] -> [BoxProduct a b]
reduceBoxProductList = fixedPointBy eqLength reduce'
    where
        eqLength a b = length a == length b
        reduce' [] = []
        reduce' (a:as) = merged_a:(reduce' rest)
            where
                merged_a = foldl merge a $ filter (== a) as
                rest = filter (/= a) as
                merge (BoxProduct a) (BoxProduct b) = boxFromList $ a ++ b

extendBoxProduct :: (Logic a, Logic b) => FreeProduct a b -> BoxProduct a b -> BoxProduct a b
extendBoxProduct a@(FreeProd _ _) (BoxProduct bs) = boxFromList $ nub $ concat $ map (fromFreeProduct' . (a <+>)) bs

extendBoxProductList a@(FreeProd _ _) bps = 
        reduceBoxProductList $ filter (/= BoxProduct []) $ map (extendBoxProduct a) $ filter (a `notInClass`) bps

inClass :: (Logic a, Logic b) => FreeProduct a b -> BoxProduct a b -> Bool
inClass a (BoxProduct bs) = a `elem` bs

notInClass :: (Logic a, Logic b) => FreeProduct a b -> BoxProduct a b -> Bool
notInClass a b = not $ inClass a b

instance (AtomicLogic a, AtomicLogic b) => Logic (BoxProduct a b) where
        one = fromFreeProduct $ one <> one
        zero = fromFreeProduct $ zero <> zero
        ortho a
            | a == zero = one
            | a == one = zero 
            | otherwise = (head . minimal . orthoCandidates) a

orthoCandidates :: (AtomicLogic a, AtomicLogic b) => BoxProduct a b -> [BoxProduct a b]
orthoCandidates a = filter (addsToOne a) $ drop 2 elements
    where
        addsToOne (BoxProduct (a:_)) (BoxProduct (b:_)) = (reduceAll $ a <+> b) == just_one
        --any (== just_one) [reduceAll $ a <+> b | a <- as, b <- bs]
        just_one = Just $ one <> one

instance (AtomicLogic a, AtomicLogic b) => AtomicLogic (BoxProduct a b) where
        atoms = [fromFreeProduct $ a <> b | a <- atoms, b <- atoms]

instance (AtomicLogic a, AtomicLogic b) => Finite (BoxProduct a b) where
        elements = zerozero:(oneone:others)
            where
                zerozero = fromFreeProduct $ zero <> zero
                oneone = fromFreeProduct $ one <> one
                others = concat $ takeWhile ((> 1) . length) $ iterate (extendByList pairs) atoms 
                pairs = [a <> b | a <- atoms, b <- atoms]

extendByList :: (Logic a, Logic b) => [FreeProduct a b] -> [BoxProduct a b] -> [BoxProduct a b]
extendByList frees boxes = reduceBoxProductList $ concat $ map (`extend` boxes) frees
extend a@(FreeProd _ _) boxes = 
    reduceBoxProductList $ filter (/= BoxProduct []) $ map (extendBoxProduct a) $ filter (a `notLessThan`) boxes
    where
        notLessThan a c = not $ lessThan a c
        lessThan a (BoxProduct c) = any (a .<.) c
-- Test cases
--
freex0one = (X0 <> X0) <+> (X0 <> X1)
x0one = boxFromList [(X0 <> One), (X0 <> X0) <+> (X0 <> X1), (X0 <> Y0) <+> (X0 <> Y1)]
x0x0 = boxFromList [(X0 <> X0)]

l4l4elements = elements :: [BoxProduct Lattice4 Lattice4]
l4l4a = atoms :: [BoxProduct Lattice4 Lattice4]
l4l4fpairs = [a <> b | a <- (atoms :: [Lattice4]), b <- (atoms :: [Lattice4])]
-- a = boxFromList [(X0 <> X1) <+> (X0 <> X0)]
-- b = boxFromList [(X1 <> X1) <+> (X1 <> X0)]

-- exprEq a@(BoxProd a1 a2) b@(BoxProd b1 b2) = a1 == b1 && a2 == b2
-- exprEq (BoxPlus a b) (BoxPlus c d) = a == c && b == d
-- exprEq _ _ = False
-- 
-- atomsLessThan :: (Atomic a, Logic a, Poset a, Atomic b, Logic b, Poset b) => BoxProduct a b -> [BoxProduct a b]
-- atomsLessThan a = filter (.<. a) atoms

-- instance (Atomic a, Logic a, Logic b, Atomic b) => Atomic (BoxProduct a b) where
--         atoms = [a <> b | a <- atoms, b <- atoms]
-- 
-- -- instance (Atomic a, Logic a, Atomic b, Logic b) => Finite (BoxProduct a b) where
-- --         elements = [zero <> zero, one <> one] ++ atoms ++ 
-- 
-- 
-- addAtoms :: (Atomic a, Logic a, Atomic b, Logic b) => [BoxProduct a b] -> [BoxProduct a b]
-- addAtoms [] = atoms
-- addAtoms as = nub $ catMaybes $ [reduceAll $ a <+> p | a <- atoms, p <- as, (not $ a .<. p)]
-- 
-- isAtom a = any (exprEq a) atoms

-- instance (Logic a, Logic b) => Poset (BoxProduct a b) where
--         (.<.) (BoxProd a1 a2) (BoxProd b1 b2) = a1 .<. b1 && a2 .<. b2 
--         -- (.<.) a@(BoxProd a1 a2) b@(BoxProd b1 b2)
--         --     | isAtom a = (a1 .<. b1) && (a2 .<. b2)
--         --     | otherwise = (atomsLessThan a) `subsetOf` (atomsLessThan b)
--         -- (.<.) a@(BoxProd _ _) (BoxPlus b c) = (a .<. b) || (a .<. c)
--         -- (.<.) (BoxPlus a b) c = (a .<. c) && (b .<. c)
-- 
-- 
-- expandBy :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> BoxProduct a b
-- expandBy a@(BoxProd a1 a2) b@(BoxProd b1 b2) 
--     | b .<. a = b <+> b1cb2 <+> b1b2c <+> b1cb2c
--     | otherwise = a
--     where
--         b1cb2 = (a1 <-> b1) <> b2
--         b1b2c = b1 <> (a2 <-> b2)
--         b1cb2c = (a1 <-> b1) <> (a2 <-> b2)
-- -- expandBy (BoxPlus a b) c@(BoxProd _ _) = (a `boxExpandBy` c) (b `boxExpandBy` c)
-- expandBy a@(BoxProd _ _) (BoxPlus b c) = (a `expandBy` b) `expandBy` c
-- -- expandBy p@(BoxPlus _ _) (BoxPlus a b) = (p `boxExpandBy` a) `boxExpandBy` b
-- 
-- subsetOf :: (Eq a) => [a] -> [a] -> Bool
-- a `subsetOf` b = all (`elem` b) a
-- 
-- reduceAllList :: (Atomic a, Logic a, Atomic b, Logic b) => BoxProduct a b -> [Maybe (BoxProduct a b)]
-- reduceAllList a = fixedPointListBy (lifted exprEq) (reduce =<<) $ Just a
--     where
--         lifted f (Just a) (Just b) = f a b
--         lifted _ Nothing Nothing = True
--         lifted _ _ _ = False
-- 
-- -- plus :: (Logic a, Logic b) => BoxProduct a b -> Equivalent (BoxProduct a b) -> Equivalent (BoxProduct a b)
-- -- plus a@(BoxProd _ _) b@(Equivalent bs) = foldl1 mergeEquivalenceClass $ map (reduceList . (a <+>)) $ bs
-- -- 
-- -- reduceList :: (Logic a, Logic b) => BoxProduct a b -> Equivalent (BoxProduct a b)
-- -- reduceList a@(BoxProd _ _) = Equivalent [a]
-- reduceList p@(BoxPlus a@(BoxProd _ _) b@(BoxProd _ _))
--     | c == Nothing = Equivalent []
--     | otherwise = Equivalent $ nub $ [p, (fromJust c)]
--     where
--         c = reduce p
-- reduceList p@(BoxPlus a@(BoxProd _ _) (BoxPlus b c)) 
--     | reduce_head == Nothing = Equivalent []
--     | otherwise = extendEquivalenceClass reduce_rest $ fromJust reduce_head
--     where
--         reduce_head = liftM (<+> c) $ reduce $ a <+> b
--         reduce_rest = prependPlus b $ reduceList $ a <+> c
-- 
-- prependPlus :: (Logic a, Logic b) => BoxProduct a b -> Equivalent (BoxProduct a b) -> Equivalent (BoxProduct a b)
-- prependPlus a@(BoxProd _ _) (Equivalent list) = Equivalent $ map (a <+>) list 

-- reduceList :: (Logic a, Logic b) => BoxProduct a b -> Equivalent (BoxProduct a b)
-- reduceList a@(BoxProd _ _) = Equivalent [a]
-- reduceList (BoxPlus a b) = reduceList' a b

---
-- boxLess :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Bool
-- boxLess a b = boxLess' (a `boxExpandBy` b) (b `boxExpandBy` a)
-- 
-- boxLess' (BoxProd a1 a2) (BoxProd b1 b2) = (a1 .<. b1) && (a2 .<. b2)
-- boxLess' a@(BoxProd _ _) (BoxPlus b c) = (a `boxLess'` b) || (a `boxLess'` c)
-- boxLess' (BoxPlus a b) c@(BoxProd _ _) = (a `boxLess'` c) && (b `boxLess'` c)
-- boxLess' p@(BoxPlus _ _) q = all (`boxLess'` q) $ boxPlusToList  p --`boxExpandBy` q
-- 
-- boxExpandBy :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> BoxProduct a b
-- boxExpandBy a@(BoxProd a1 a2) b@(BoxProd b1 b2) 
--     | b `boxLess'` a = boxNormalForm $ BoxPlus b $ BoxPlus b1cb2 $ BoxPlus b1b2c b1cb2c
--     | otherwise = a
--     where
--         b1cb2 = (a1 <-> b1) <> b2
--         b1b2c = b1 <> (a2 <-> b2)
--         b1cb2c = (a1 <-> b1) <> (a2 <-> b2)
-- boxExpandBy (BoxPlus a b) c@(BoxProd _ _) = boxNormalForm $ BoxPlus (a `boxExpandBy` c) (b `boxExpandBy` c)
-- boxExpandBy a@(BoxProd _ _) (BoxPlus b c) = boxNormalForm $ (a `boxExpandBy` b) `boxExpandBy` c
-- boxExpandBy p@(BoxPlus _ _) (BoxPlus a b) = (p `boxExpandBy` a) `boxExpandBy` b
-- 
-- boxPlus :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> BoxProduct a b
-- boxPlus a@(BoxProd a1 a2) b@(BoxProd b1 b2)
--     | a `boxLess` b = b
--     | b `boxLess` a = a
--     | a1 == b1 = if a2 `isOrthogonal` b2 then a1 <> (a2 \./ b2) else one <> one
--     | a2 == b2 = if a1 `isOrthogonal` b1 then (a1 \./ b1) <> a2 else one <> one
--     | (a1 /= b1) && (a2 `isOrthogonal` b2) = BoxPlus a b
--     | (a2 /= b2) && (a1 `isOrthogonal` b1) = BoxPlus a b
--     | otherwise = BoxProd one one
-- boxPlus a@(BoxProd _ _) p@(BoxPlus b rest) = boxPlusReduce (boxPlus a b) rest
-- boxPlus p@(BoxPlus _ _) a@(BoxProd _ _) = boxPlus a p
-- boxPlus (BoxPlus a b) p@(BoxPlus _ _) = boxPlus (boxPlus a p) b
-- 
-- boxPlusReduce a@(BoxProd _ _) p = boxPlus a p
-- boxPlusReduce (BoxPlus a as) p = boxPlus as $ boxPlus a p
-- 
-- safeBoxPlus :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Maybe (BoxProduct a b)
-- safeBoxPlus a@(BoxProd a1 a2) b@(BoxProd b1 b2)
--     | a `boxLess` b = Just b
--     | b `boxLess` a = Just a
--     | a1 == b1 = if a2 `isOrthogonal` b2 then Just (a1 <> (a2 \./ b2)) else Nothing
--     | a2 == b2 = if a1 `isOrthogonal` b1 then Just ((a1 \./ b1) <> a2) else Nothing
--     | (a1 /= b1) && (a2 `isOrthogonal` b2) = Just $ BoxPlus a b
--     | (a2 /= b2) && (a1 `isOrthogonal` b1) = Just $ BoxPlus a b
--     | otherwise = Nothing
-- safeBoxPlus a@(BoxProd _ _) p@(BoxPlus b rest) = (safeBoxPlus a b) >>= (`safeBoxPlusReduce` rest)
-- safeBoxPlus p@(BoxPlus _ _) a@(BoxProd _ _) = safeBoxPlus a p
-- safeBoxPlus (BoxPlus a b) p@(BoxPlus _ _) = (safeBoxPlus a p) >>= (`safeBoxPlus` b)
-- 
-- safeBoxPlusReduce :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Maybe (BoxProduct a b)
-- safeBoxPlusReduce a@(BoxProd _ _) p = safeBoxPlus a p
-- safeBoxPlusReduce (BoxPlus a as) p = (safeBoxPlus as) =<< (safeBoxPlus a p)
-- 
-- (<+>) :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> BoxProduct a b
-- (<+>) = boxPlus
-- 
-- boxNormalForm :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b
-- boxNormalForm p 
--     | length terms > 0 = foldl1 (BoxPlus) terms
--     | otherwise = BoxProd zero zero
--     where
--         terms = filter dropZero $ boxPlusToList p
--         dropZero (BoxProd a b)
--             | a == zero && b == zero = False
--             | otherwise = True
-- 
-- boxReduce :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b
-- boxReduce p = foldl1 (boxPlus) $ terms
--     where
--         terms = nub $ filter (/= z) $ boxPlusToList p
--         z = BoxProd zero zero
-- 
-- boxPlusToList :: (Logic a, Logic b) => BoxProduct a b -> [BoxProduct a b]
-- boxPlusToList a@(BoxProd _ _) = [a]
-- boxPlusToList (BoxPlus a b) = boxPlusToList a ++ boxPlusToList b
-- 
-- instance (Logic a, Logic b) => Eq (BoxProduct a b) where
--         (==) a b = (a `boxLess` b) && (b `boxLess` a)
-- 
-- allBoxProducts :: (Logic a, Logic b) => [BoxProduct a b]
-- allBoxProducts = [boxProd a b | a <- elements, b <- elements]
-- 
-- allBoxSums :: (Logic a, Logic b) => [BoxProduct a b] -> [BoxProduct a b]
-- allBoxSums [] = allBoxProducts
-- allBoxSums as = nub $ [a `boxPlus` b | a <- as, b <- allBoxProducts]
-- 
-- instance (Logic a, Logic b) => Finite (BoxProduct a b) where
--         elements = stable $ iterate allBoxSums allBoxProducts  
--             where
--                 stable (x:ys@(y:_))
--                     | length x == length y = y
--                     | otherwise = stable ys
-- 
-- instance (Logic a, Logic b) => Poset (BoxProduct a b) where
--         (.<.) = boxLess
-- 
-- instance (Logic a, Logic b) => Logic (BoxProduct a b) where
--         one = BoxProd one one
--         zero = BoxProd zero zero
--         ortho q = head $ filter (\p -> (isBoxOrthogonal q p) && (p `safeBoxPlus` q == Just one)) elements
-- 
-- isBoxOrthogonal :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Bool
-- isBoxOrthogonal (BoxProd a1 a2) (BoxProd b1 b2) = (a1 `isOrthogonal` b1) || (a2 `isOrthogonal` b2)
-- isBoxOrthogonal a@(BoxProd _ _) (BoxPlus c d) = (a `isBoxOrthogonal` c) && (a `isBoxOrthogonal` d)
-- isBoxOrthogonal p@(BoxPlus _ _) a@(BoxProd _ _) = a `isBoxOrthogonal` p
-- isBoxOrthogonal (BoxPlus a b) p@(BoxPlus _ _) = (a `isBoxOrthogonal` p) && (b `isBoxOrthogonal` p)

(<->) :: (Logic a) => a -> a -> a
b <-> a = fromJust $ b /\ ortho a

listCycleBy :: (a -> a -> Bool) -> [a] -> a
listCycleBy _ (x:[]) = x
listCycleBy eq xs =  firstCycle' [] xs
    where
        firstCycle' prefix (x:xs)
            | any (eq x) prefix = x
            | otherwise = firstCycle' (x:prefix) xs

firstCycleBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
firstCycleBy eq f x0 = listCycleBy eq $ iterate f x0

fixedPointListBy :: (a -> a -> Bool) -> (a -> a) -> a -> [a]
fixedPointListBy eq f x0 = takeUntilStable $ iterate f x0
    where
        takeUntilStable (x:ys@(y:_))
            | x `eq` y = [y]
            | otherwise = x:(takeUntilStable ys)

fixedPointBy :: (a -> a -> Bool) -> (a -> a) -> a -> a 
fixedPointBy eq f x0 = stable $ iterate f x0
    where
        stable (x:ys@(y:_))
            | x `eq` y = y
            | otherwise = stable ys

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint = fixedPointBy (==) 

