module QLogic.BoxWorld where

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
        
singleSystem = elements :: [Lattice4]
doubleSystem = elements :: [Product Lattice4 Lattice4]
pastedSystem = elements :: [ZeroOnePasting Lattice4 Lattice4]

-- data FreeProduct a b = FreeProd a b | FreePlus [FreeProduct a b] deriving (Show) 

data BoxProduct a b = BoxProd a b | BoxPlus (BoxProduct a b) (BoxProduct a b)

instance (Show a, Show b) => Show (BoxProduct a b) where
        show (BoxProd a b) = "(" ++ (show a) ++ (show b) ++ ")"
        show (BoxPlus a p) = (show a) ++ "⊕" ++ (show p)

(<>) :: (Logic a, Logic b) => a -> b -> BoxProduct a b
(<>) = boxProd

boxLess :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Bool
boxLess (BoxProd a1 a2) (BoxProd b1 b2) = (a1 .<. b1) && (a2 .<. b2)
boxLess a@(BoxProd _ _) (BoxPlus b c) = (a `boxLess` b) || (a `boxLess` c)
boxLess (BoxPlus a b) c@(BoxProd _ _) = (a `boxLess` c) && (b `boxLess` c)
boxLess p@(BoxPlus _ _) q@(BoxPlus _ _) = all (`boxLess` q) $ boxPlusToList $ p `boxExpandBy` q

boxExpandBy :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> BoxProduct a b
boxExpandBy a@(BoxProd a1 a2) b@(BoxProd b1 b2) 
    | b `boxLess` a = boxNormalForm $ BoxPlus b $ BoxPlus b1cb2 $ BoxPlus b1b2c b1cb2c
    | otherwise = a
    where
        b1cb2 = boxProd (a1 <-> b1) b2
        b1b2c = boxProd b1 (a2 <-> b2)
        b1cb2c = boxProd (a1 <-> b1) (a2 <-> b2)
boxExpandBy (BoxPlus a b) c@(BoxProd _ _) = boxNormalForm $ BoxPlus (a `boxExpandBy` c) (b `boxExpandBy` c)
boxExpandBy p@(BoxPlus _ _) (BoxPlus a b) = (p `boxExpandBy` a) `boxExpandBy` b

boxProd :: (Logic a, Logic b) => a -> b -> BoxProduct a b
boxProd a b
    | (a == zero) || (b == zero) = BoxProd zero zero
    | otherwise = BoxProd a b

boxPlus :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> BoxProduct a b
boxPlus a@(BoxProd a1 a2) b@(BoxProd b1 b2)
    | a `boxLess` b = b
    | b `boxLess` a = a
    | a1 == b1 = if a2 `isOrthogonal` b2 then boxProd a1 (a2 \./ b2) else boxProd one one
    | a2 == b2 = if a1 `isOrthogonal` b1 then boxProd (a1 \./ b1) a2 else boxProd one one
    | (a1 /= b1) && (a2 `isOrthogonal` b2) = BoxPlus a b
    | (a2 /= b2) && (a1 `isOrthogonal` b1) = BoxPlus a b
    | otherwise = BoxProd one one
boxPlus a@(BoxProd _ _) p@(BoxPlus b rest) = boxPlusReduce (boxPlus a b) rest
boxPlus p@(BoxPlus _ _) a@(BoxProd _ _) = boxPlus a p
boxPlus (BoxPlus a b) p@(BoxPlus _ _) = boxPlus (boxPlus a p) b

boxPlusReduce a@(BoxProd _ _) p = boxPlus a p
boxPlusReduce (BoxPlus a as) p = boxPlus as $ boxPlus a p

(<+>) :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> BoxProduct a b
(<+>) = boxPlus

boxNormalForm :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b
boxNormalForm p = foldl1 (BoxPlus) terms
    where
        terms = nub $ filter (/= z) $ boxPlusToList p
        z = BoxProd zero zero

boxReduce :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b
boxReduce p = foldl1 (boxPlus) $ terms
    where
        terms = nub $ filter (/= z) $ boxPlusToList p
        z = BoxProd zero zero

boxPlusToList :: (Logic a, Logic b) => BoxProduct a b -> [BoxProduct a b]
boxPlusToList a@(BoxProd _ _) = [a]
boxPlusToList (BoxPlus a b) = boxPlusToList a ++ boxPlusToList b

instance (Logic a, Logic b) => Eq (BoxProduct a b) where
        (==) a b = (a `boxLess` b) && (b `boxLess` a)

allBoxProducts :: (Logic a, Logic b) => [BoxProduct a b]
allBoxProducts = nub $ [boxProd a b | a <- elements, b <- elements]

allBoxSums :: (Logic a, Logic b) => [BoxProduct a b] -> [BoxProduct a b]
allBoxSums [] = allBoxProducts
allBoxSums as = nub $ [a `boxPlus` b | a <- as, b <- allBoxProducts]

instance (Logic a, Logic b) => Finite (BoxProduct a b) where
        elements = stable $ iterate allBoxSums allBoxProducts  
            where
                stable (x:ys@(y:_))
                    | length x == length y = y
                    | otherwise = stable ys

instance (Logic a, Logic b) => Poset (BoxProduct a b) where
        (.<.) = boxLess

instance (Logic a, Logic b) => Logic (BoxProduct a b) where
        one = BoxProd one one
        zero = BoxProd zero zero
        ortho q = head $ filter (\p -> (isBoxOrthogonal q p) && (p <+> q == one)) elements
        -- ortho (BoxProd a1 a2) = (a1 <> ortho a2) <+> (ortho a1 <> a2) <+> (ortho a1 <> ortho a2)
        -- ortho (BoxPlus a b) = (ortho a) /.\ (ortho b)

isBoxOrthogonal :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Bool
isBoxOrthogonal (BoxProd a1 a2) (BoxProd b1 b2) = (a1 `isOrthogonal` b1) || (a2 `isOrthogonal` b2)
isBoxOrthogonal a@(BoxProd _ _) (BoxPlus c d) = (a `isBoxOrthogonal` c) && (a `isBoxOrthogonal` d)
isBoxOrthogonal p@(BoxPlus _ _) a@(BoxProd _ _) = a `isBoxOrthogonal` p
isBoxOrthogonal (BoxPlus a b) p@(BoxPlus _ _) = (a `isBoxOrthogonal` p) && (b `isBoxOrthogonal` p)

--
-- The following is incomplete (i.e. sth like a free product)
--
-- Maybe it will be useful to revise it in the future
-- with more efficient implementation (the current one is highly
-- inefficient, while the problem shouldn't be so complicated)
-- and more 'free' approach. 
-- 

data FreeProduct a b = FreeProd a b | FreePlus (FreeProduct a b) (FreeProduct a b)

instance (Show a, Show b) => Show (FreeProduct a b) where
        show (FreeProd a b) = "(" ++ (show a) ++ (show b) ++ ")"
        show (FreePlus a p) = (show a) ++ "⊕" ++ (show p)

freePlus :: (Ord a, Ord b) => FreeProduct a b -> FreeProduct a b -> FreeProduct a b
freePlus a@(FreeProd a1 a2) b@(FreeProd b1 b2) 
    | (a1, a2) < (b1, b2) = FreePlus a b
    | (a1, a2) == (b1, b2) = a
    | otherwise = FreePlus b a
freePlus a@(FreeProd a1 a2) b@(FreePlus p@(FreeProd p1 p2) ps)
    | (a1, a2) < (p1, p2) = FreePlus a b
    | (a1, a2) == (p1, p2) = b
    | otherwise = FreePlus p (freePlus a ps)
freePlus (FreePlus a as) b@(FreePlus _ _) = freePlus a (freePlus as b)
freePlus p@(FreePlus _ _) b@(FreeProd _ _) = freePlus b p

contains :: (Logic a, Logic b) => FreeProduct a b -> FreeProduct a b -> Bool
(FreeProd a1 a2) `contains` (FreeProd b1 b2) = (a1 == b1 && a2 .>. b2 && a2 /= b2) ||
                                               (a2 == b2 && a1 .>. b1 && a1 /= b1)
(FreePlus a as) `contains` b@(FreeProd _ _) = a `contains` b || as `contains` b
a@(FreePlus _ _) `contains` (FreePlus b bs) = a `contains` b && a `contains` bs

expandBy :: (Ord a, Ord b, Logic a, Logic b) => FreeProduct a b -> FreeProduct a b -> FreeProduct a b
a@(FreeProd a1 a2) `expandBy` (FreeProd b1 b2)
    | a1 .>. b1 && a1 /= b1 && a2 == b2 = (b1 `FreeProd` a2) `freePlus` ((a1 <-> b1) `FreeProd` a2)
    | a2 .>. b2 && a2 /= b2 && a1 == b1 = (a1 `FreeProd` b2) `freePlus` (a1 `FreeProd` (a2 <-> b2))
    | otherwise = a
p@(FreePlus a as) `expandBy` b@(FreeProd _ _) = freePlus (a `expandBy` b) (as `expandBy` b)
p@(FreePlus _ _) `expandBy` (FreePlus b bs) = freePlus (p `expandBy` b) (p `expandBy` bs)

freePlusToList :: (Logic a, Logic b) => FreeProduct a b -> [FreeProduct a b]
freePlusToList a@(FreeProd _ _) = [a]
freePlusToList p@(FreePlus a as) = a:(freePlusToList as)

-- freeLess :: (Ord a, Ord b, Logic a, Logic b) => FreeProduct a b -> FreeProduct a b -> Bool
-- a@(FreeProd a1 a2) `freeLess` b@(FreeProd b1 b2) = (a1 .<. b1 && a2 .<. b2

reduce :: (Ord a, Ord b, Logic a, Logic b) => FreeProduct a b -> FreeProduct a b
reduce a@(FreeProd _ _) = a
reduce (FreePlus a as) = reduce' a $ reduce as

reduce' :: (Ord a, Ord b, Logic a, Logic b) => FreeProduct a b -> FreeProduct a b -> FreeProduct a b
reduce' accum@(FreeProd a1 a2) b@(FreeProd b1 b2)
    | a1 .<. b1 && a2 .<. b2 = b
    | a1 .>. b1 && a2 .>. b2 = accum
    | a1 == b1 && a2 `isOrthogonal` b2 = a1 `FreeProd` (a2 \./ b2)
    | a2 == b2 && a1 `isOrthogonal` b1 = (a1 \./ b1) `FreeProd` a2
    | otherwise = accum `freePlus` b 
reduce' accum@(FreePlus a as) b@(FreeProd b1 b2) = (reduce' a b) `freePlus` as
reduce' accum@(FreeProd a1 a2) (FreePlus b bs) = reduce' (reduce' accum b) bs
reduce' accum@(FreePlus _ _) (FreePlus b bs) = reduce' (reduce' accum b) bs

instance (Ord a, Ord b, Logic a, Logic b) => Eq (FreeProduct a b) where
        a@(FreeProd a1 a2) == b@(FreeProd b1 b2)
            | (a1 == zero || a2 == zero) && (b1 == zero || b2 == zero) = True
            | a1 == b1 && a2 == b2 = True
            | otherwise = False
        b@(FreeProd _ _) == p@(FreePlus _ _) = p == b
        p@(FreePlus a as) == b@(FreeProd _ _)
            | b `contains` a = p == (b `expandBy` a)
            | otherwise = False
        p@(FreePlus a as) == q@(FreePlus b bs) 
            | a == b = as == bs
            | p `contains` b = (p `expandBy` b) == q
            | q `contains` a = p == (q `expandBy` a)
            | otherwise = False

-- instance Finite (FreeProduct a b) where
--         elements = removeDuplicates 


allFreeProducts :: (Ord a, Ord b, Logic a, Logic b) => [FreeProduct a b]
allFreeProducts = nub $ [FreeProd a b | a <- elements, b <- elements]

formalSums as bs = nub $ [reduce $ a `freePlus` b | a <- as, b <- bs]

(<->) :: (Logic a) => a -> a -> a
b <-> a = fromJust $ b /\ ortho a
