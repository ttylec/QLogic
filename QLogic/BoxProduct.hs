module QLogic.BoxProduct where

import QLogic
import QLogic.Tools
import Data.List
import Data.Maybe
import Data.Function
import Control.Monad
import Data.Typeable

import Control.Parallel
import Control.Parallel.Strategies

import Control.DeepSeq

instance NFData (BoxProduct a b) where
        rnf (BoxProduct a) = rnf a `seq` ()

instance NFData (FreeProduct a b) where

data FreeProduct a b = FreeProd a b | FreePlus (FreeProduct a b) (FreeProduct a b)

instance (Show a, Show b) => Show (FreeProduct a b) where
        show (FreeProd a b) = (show a) ++ (show b)
        show (FreePlus a p) = (show a) ++ "⊕" ++ (show p)

instance (Repr a, Repr b) => Repr (FreeProduct a b) where
        repr (FreeProd a b) = (repr a) ++ (repr b)
        repr (FreePlus a p) = (repr a) ++ "plus" ++ (repr p)

instance (Poset a, Poset b) => Eq (FreeProduct a b) where
        a == b = a .<. b && b .<. a

instance (Poset a, Poset b) => Poset (FreeProduct a b) where
        (FreeProd a1 a2) .<. (FreeProd b1 b2) = a1 .<. b1 && a2 .<. b2
        a@(FreeProd _ _) .<. (FreePlus b bs) = a .<. b || a .<. bs
        (FreePlus a as) .<. b = a .<. b && as .<. b

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

data BoxProduct a b = BoxProduct [FreeProduct a b]

instance (Show a, Show b) => Show (BoxProduct a b) where
        show (BoxProduct []) = "Not exists"
        show (BoxProduct a) = "⟨" ++ (show $ head a) ++ "⟩"

instance (Repr a, Repr b) => Repr (BoxProduct a b) where
        repr (BoxProduct []) = "Not exists"
        repr (BoxProduct a) = repr (head a)

instance (Logic a, Logic b) => Eq (BoxProduct a b) where
        (BoxProduct []) == (BoxProduct []) = True
        (BoxProduct as) == (BoxProduct bs) = {-# SCC bpeq #-} any (`elem` bs) as

instance (Logic a, Logic b) => Poset (BoxProduct a b) where
        (BoxProduct as) .<. (BoxProduct bs) = any id [a .<. b | a <- as, b <- bs]

instance (AtomicLogic a, AtomicLogic b) => Finite (BoxProduct a b) where
        elements = reduceBoxProductList $ zerozero:(oneone:others)
            where
                zerozero = fromFreeProduct $ zero <> zero
                oneone = fromFreeProduct $ one <> one
                others = concat $ takeWhile ((> 1) . length) $ iterate (extendByList pairs) atoms 
                pairs = [a <> b | a <- atoms, b <- atoms]
                bppairs = map fromFreeProduct pairs

instance (AtomicLogic a, AtomicLogic b) => Logic (BoxProduct a b) where
        one = fromFreeProduct $ one <> one
        zero = fromFreeProduct $ zero <> zero
        ortho a
            | a == zero = one
            | a == one = zero 
            | otherwise = boxOrtho a
        orthoIn el a
            | a == zero = one
            | a == one = zero
            | otherwise = boxOrthoIn el a

instance (AtomicLogic a, AtomicLogic b) => AtomicLogic (BoxProduct a b) where
        atoms = [fromFreeProduct $ a <> b | a <- atoms, b <- atoms]

boxFromList :: (Logic a, Logic b) => [FreeProduct a b] -> BoxProduct a b
boxFromList = BoxProduct . nub

boxToList :: (Logic a, Logic b) => BoxProduct a b -> [FreeProduct a b]
boxToList (BoxProduct as) = as

boxFromReprIn :: (AtomicLogic a, AtomicLogic b) => [BoxProduct a b] -> FreeProduct a b -> BoxProduct a b
boxFromReprIn el a 
    | reduced == Nothing = BoxProduct []
    | otherwise = let r = fromJust reduced in 
                      head $ filter (r `inClass`) el
    where
        reduced = reduceAll a

boxFromRepr :: (AtomicLogic a, AtomicLogic b) => FreeProduct a b -> BoxProduct a b
boxFromRepr a
    | reduced == Nothing = BoxProduct []
    | otherwise = let r = fromJust reduced in 
                      head $ filter (r `inClass`) elements
    where
        reduced = reduceAll a

boxToRepr :: (AtomicLogic a, AtomicLogic b) => BoxProduct a b -> FreeProduct a b
boxToRepr (BoxProduct as) = foldl1 (\accum a -> if len a > len accum then a else accum) as
    where
        len = length . freeToList

reduce :: (Logic a, Logic b) => FreeProduct a b -> Maybe (FreeProduct a b)
reduce a@(FreeProd _ _) = Just a
reduce (FreePlus a@(FreeProd a1 a2) b@(FreeProd b1 b2))
    | a .<. b = Just b
    | b .<. a = Just a
    | a1 == b1 = if a2 -|- b2 then (Just $ a1 <> (a2 \./ b2)) else Nothing
    | a2 == b2 = if a1 -|- b1 then (Just $ (a1 \./ b1) <> a2) else Nothing
    | (a1 /= b1) && (a2 -|- b2) = Just $ a <+> b
    | (a2 /= b2) && (a1 -|- b1) = Just $ a <+> b
    | otherwise = Nothing
reduce (FreePlus a@(FreeProd _ _) (FreePlus b c)) = reduce (a <+> b) >>= (`reduce'` c)
    where
        reduce' a@(FreeProd _ _) p = Just $ a <+> p
        reduce' (FreePlus a b) p = liftM (b <+>) $ reduce $ a <+> p

reduceAll :: (Logic a, Logic b) => FreeProduct a b -> Maybe (FreeProduct a b)
reduceAll a 
    | admissible alist = liftM freeFromList $ fixedPointBy (lifted eqLen) (reduceLeftRight' =<<) $ Just $ alist
    | otherwise = Nothing
    where
        alist = freeToList a
        admissible (a0:[]) = True
        admissible (a0:as) = (isJust $ sequence $ map (reduce . (a0 <+>)) as) && admissible as

        lifted f (Just a) (Just b) = f a b
        lifted _ Nothing Nothing = True
        lifted _ _ _ = False

        eqLen a b = length a == length b

reduceLeftRight' :: (Logic a, Logic b) => [FreeProduct a b] -> Maybe [FreeProduct a b]
reduceLeftRight' a = (Just $ reduceSubOrdered a) >>= reduceLeft' >>= reduceRight'

reduceSubOrdered :: (Logic a, Logic b) => [FreeProduct a b] -> [FreeProduct a b]
reduceSubOrdered [] = []
reduceSubOrdered (a0:as) 
    | any (a0 .<. ) as = reduceSubOrdered as
    | otherwise = a0:(reduceSubOrdered $ filter (not . (a0 .>.)) as)

reduceLeft' :: (Logic a, Logic b) => [FreeProduct a b] -> Maybe [FreeProduct a b]
reduceLeft' [] = Just []
reduceLeft' (a0:[]) = Just [a0]
reduceLeft' (a0@(FreeProd a0left a0right):as)
    | (not . mutuallyOrthogonal) a0slefts = Nothing
    | otherwise = liftM (reduced:) $ reduceLeft' rest
    where
        (a0s, rest) = partition ((a0right ==) . rightComponent) as
        reduced = (foldl (\./) a0left a0slefts) <> a0right
        a0slefts = map leftComponent a0s

reduceRight' [] = Just [] 
reduceRight' (a0:[]) = Just [a0]
reduceRight' (a0@(FreeProd a0left a0right):as)
    | (not . mutuallyOrthogonal) a0srights = Nothing
    | otherwise = liftM (reduced:) $ reduceRight' rest
    where
        (a0s, rest) = partition ((a0left ==) . leftComponent) as
        reduced = a0left <> (foldl (\./) a0right a0srights)
        a0srights = map rightComponent a0s

rightComponent (FreeProd a1 a2) = a2
leftComponent (FreeProd a1 a2) = a1

fromFreeProduct :: (Logic a, Logic b) => FreeProduct a b -> BoxProduct a b
fromFreeProduct = boxFromList . fromFreeProduct' 
        
fromFreeProduct' :: (Logic a, Logic b) => FreeProduct a b -> [FreeProduct a b]
fromFreeProduct' a 
    | reduced == Nothing = []
    | otherwise = [a, (fromJust reduced)]
    where
        reduced = reduceAll a

reduceBoxProductList :: (Logic a, Logic b) => [BoxProduct a b] -> [BoxProduct a b]
reduceBoxProductList as = fixedPointBy eqLength (reduceByHead) $ filter (/= BoxProduct []) as
    where
        eqLength a b = length a == length b
        reduce' accum [] = accum
        reduce' accum (a:as) = reduce' (merged_a:accum) rest
            where
                merged_a = foldl' merge a $ filter (== a) as
                rest = filter (/= a) as
                merge (BoxProduct a) (BoxProduct b) = boxFromList $ a ++ b

reduceByHead :: (Logic a, Logic b) => [BoxProduct a b] -> [BoxProduct a b]
reduceByHead [] = []
reduceByHead (a:[]) = [a]
reduceByHead (a:as) = b0:(reduceByHead bs)
    where
        (b0:bs) = foldl' mergeOnHead [a] as
        mergeOnHead (h:rest) b
            | h == b = (merge h b):rest
            | otherwise = h:b:rest
        merge (BoxProduct a) (BoxProduct b) = boxFromList $ a ++ b

extendBoxProduct :: (Logic a, Logic b) => FreeProduct a b -> BoxProduct a b -> BoxProduct a b
extendBoxProduct a@(FreeProd _ _) (BoxProduct bs) = boxFromList $ nub $ concat $ map (fromFreeProduct' . (a <+>)) bs

extendBoxProductList a@(FreeProd _ _) bps = 
        reduceBoxProductList $ filter (/= BoxProduct []) $ map (extendBoxProduct a) $ filter (a `notInClass`) bps

inClass :: (Logic a, Logic b) => FreeProduct a b -> BoxProduct a b -> Bool
inClass a (BoxProduct bs) = a `elem` bs

notInClass :: (Logic a, Logic b) => FreeProduct a b -> BoxProduct a b -> Bool
notInClass a b = not $ inClass a b

orthoCandidates :: (AtomicLogic a, AtomicLogic b) => BoxProduct a b -> [BoxProduct a b]
orthoCandidates a = filter (addsToOne a) $ elements
    where
        addsToOne (BoxProduct (a:_)) (BoxProduct (b:_)) = (reduceAll $ a <+> b) == just_one
        --any (== just_one) [reduceAll $ a <+> b | a <- as, b <- bs]
        just_one = Just $ one <> one

extendByList :: (Logic a, Logic b) => [FreeProduct a b] -> [BoxProduct a b] -> [BoxProduct a b]
extendByList frees boxes = reduceBoxProductList $ concat $ (map (`extend` boxes) frees `using` parList rdeepseq)
extend a@(FreeProd _ _) boxes = 
    reduceBoxProductList $ filter (/= BoxProduct []) $ map (extendBoxProduct a) $ filter (a `notLessThan`) boxes
    where
        notLessThan a c = not $ lessThan a c
        lessThan a (BoxProduct c) = any (a .<.) c

--
-- Alternate construction of BoxProduct elements
--

testElements' :: (AtomicLogic a, AtomicLogic b) => [FreeProduct a b] -> [BoxProduct a b] -> [BoxProduct a b]
testElements' atoms els = reduceBoxProductList [boxFromFree $ a <+> b | a <- atoms, b <- reps, (not $ a .<. b)]
    where
        reps = map boxRepr els
            
testElements :: (AtomicLogic a, AtomicLogic b) => [BoxProduct a b]
testElements = reduceBoxProductList combinations
        where
            combinations = concat $ (take 3 $ iterate (testElements' freeAtoms) boxAtoms)
            freeAtoms = [a <> b | a <- atoms, b <- atoms]
            boxAtoms = map (boxFromFree) freeAtoms

boxFromFree :: (Logic a, Logic b) => FreeProduct a b -> BoxProduct a b
boxFromFree a
    | reduceList == [] = BoxProduct []
    | otherwise = boxFromList $ a:reduceList
    where
        reduceList = catMaybes $ map (reduceAll' . freeFromList) $ permutations $ freeToList a

reduceAll' :: (Logic a, Logic b) => FreeProduct a b -> Maybe (FreeProduct a b)
reduceAll' a = firstCycleBy (lifted exprEq) (reduce =<<) $ Just a
    where
        lifted f (Just a) (Just b) = f a b
        lifted _ Nothing Nothing = True
        lifted _ _ _ = False

        exprEq a@(FreeProd a1 a2) b@(FreeProd b1 b2) = a1 == b1 && a2 == b2
        exprEq (FreePlus a b) (FreePlus c d) = a == c && b == d
        exprEq _ _ = False

boxRepr :: (Logic a, Logic b) => BoxProduct a b -> FreeProduct a b
boxRepr (BoxProduct (a:_)) = a

--
-- Alternative orthocompletion
--
boxOrtho :: (AtomicLogic a, AtomicLogic b) => BoxProduct a b -> BoxProduct a b
boxOrtho = (foldl1 (/.\)) . boxOrtho' . boxRepr

boxOrtho' :: (AtomicLogic a, AtomicLogic b) => FreeProduct a b -> [BoxProduct a b]
boxOrtho' (FreeProd a b) = [boxFromRepr $ (a <> ortho b) <+> (ortho a <> b) <+> (ortho a <> ortho b)]
boxOrtho' (FreePlus a b) = concat [boxOrtho' a, boxOrtho' b]

boxOrthoIn :: (AtomicLogic a, AtomicLogic b) => [BoxProduct a b] -> BoxProduct a b -> BoxProduct a b
boxOrthoIn el a = (foldl1 (unsafeMeetIn el)) $ boxOrthoIn' el $ boxRepr a

boxOrthoIn' :: (AtomicLogic a, AtomicLogic b) => [BoxProduct a b] -> FreeProduct a b -> [BoxProduct a b]
boxOrthoIn' el (FreeProd a b) = [boxFromReprIn el $ (a <> ortho b) <+> (ortho a <> b) <+> (ortho a <> ortho b)]
boxOrthoIn' el (FreePlus a b) = concat [boxOrthoIn' el a, boxOrthoIn' el b]

--
-- Other stuff
--

(<->) :: (Logic a) => a -> a -> a
b <-> a = fromJust $ b /\ ortho a

createStaticBoxProduct :: (Repr a, Repr b, AtomicLogic a, AtomicLogic b) => String -> [BoxProduct a b] -> [BoxProduct a b] -> String
createStaticBoxProduct name at el = unlines $ ["import QLogic", dataline, finiteinstance, posetinstance, logicinstance, atomicinstance]
        where
            dataline = "data " ++ name ++ " = " ++ (intercalate " | " $ map repr el) ++ " deriving (Enum, Bounded, Eq, Show)"
            finiteinstance = "instance Finite TwoTwoBoxWorld where\n"
                                ++ indent ++ "elements = [minBound..]\n" 
            posetinstance = "instance Poset TwoTwoBoxWorld where\n" ++ (unlines $ map posetRelation el) ++ indent ++ "_ .<. _ = False\n"
            logicinstance = "instance Logic TwoTwoBoxWorld where\n"
                                ++ indent ++ "zero = " ++ (repr $ el !! 0) ++ "\n"
                                ++ indent ++ "one = " ++ (repr $ el !! 1) ++ "\n"
                                ++ (unlines $ map orthoOf el) ++ "\n"
            atomicinstance = "instance AtomicLogic TwoTwoBoxWorld where\n"
                                ++ indent ++ "atoms = [" ++ (intercalate ", " $ map repr $ at) ++ "]\n"

            posetRelation a = unlines $ map (\b -> indent ++ (repr a) ++ " .<. " ++ (repr b) ++ " = True") $ filter (a .<.) el
            orthoOf a = indent ++ "ortho " ++ (repr a) ++ " = " ++ (repr $ orthoIn el a)
            indent = "    "

