module QLogic.BoxProduct where

import QLogic
import Data.List
import Data.Maybe
import Control.Monad

data BoxProduct a b = BoxProd a b | BoxPlus (BoxProduct a b) (BoxProduct a b)

instance (Show a, Show b) => Show (BoxProduct a b) where
        show (BoxProd a b) = "(" ++ (show a) ++ (show b) ++ ")"
        show (BoxPlus a p) = (show a) ++ "⊕" ++ (show p)

boxProd :: (Logic a, Logic b) => a -> b -> BoxProduct a b
boxProd a b
    | (a == zero) || (b == zero) = BoxProd zero zero
    | otherwise = BoxProd a b

(<>) :: (Logic a, Logic b) => a -> b -> BoxProduct a b
(<>) = boxProd

boxLess :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Bool
boxLess a b = boxLess' (a `boxExpandBy` b) (b `boxExpandBy` a)

boxLess' (BoxProd a1 a2) (BoxProd b1 b2) = (a1 .<. b1) && (a2 .<. b2)
boxLess' a@(BoxProd _ _) (BoxPlus b c) = (a `boxLess'` b) || (a `boxLess'` c)
boxLess' (BoxPlus a b) c@(BoxProd _ _) = (a `boxLess'` c) && (b `boxLess'` c)
boxLess' p@(BoxPlus _ _) q = all (`boxLess'` q) $ boxPlusToList  p --`boxExpandBy` q

boxExpandBy :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> BoxProduct a b
boxExpandBy a@(BoxProd a1 a2) b@(BoxProd b1 b2) 
    | b `boxLess'` a = boxNormalForm $ BoxPlus b $ BoxPlus b1cb2 $ BoxPlus b1b2c b1cb2c
    | otherwise = a
    where
        b1cb2 = (a1 <-> b1) <> b2
        b1b2c = b1 <> (a2 <-> b2)
        b1cb2c = (a1 <-> b1) <> (a2 <-> b2)
boxExpandBy (BoxPlus a b) c@(BoxProd _ _) = boxNormalForm $ BoxPlus (a `boxExpandBy` c) (b `boxExpandBy` c)
boxExpandBy a@(BoxProd _ _) (BoxPlus b c) = boxNormalForm $ (a `boxExpandBy` b) `boxExpandBy` c
boxExpandBy p@(BoxPlus _ _) (BoxPlus a b) = (p `boxExpandBy` a) `boxExpandBy` b

boxPlus :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> BoxProduct a b
boxPlus a@(BoxProd a1 a2) b@(BoxProd b1 b2)
    | a `boxLess` b = b
    | b `boxLess` a = a
    | a1 == b1 = if a2 `isOrthogonal` b2 then a1 <> (a2 \./ b2) else one <> one
    | a2 == b2 = if a1 `isOrthogonal` b1 then (a1 \./ b1) <> a2 else one <> one
    | (a1 /= b1) && (a2 `isOrthogonal` b2) = BoxPlus a b
    | (a2 /= b2) && (a1 `isOrthogonal` b1) = BoxPlus a b
    | otherwise = BoxProd one one
boxPlus a@(BoxProd _ _) p@(BoxPlus b rest) = boxPlusReduce (boxPlus a b) rest
boxPlus p@(BoxPlus _ _) a@(BoxProd _ _) = boxPlus a p
boxPlus (BoxPlus a b) p@(BoxPlus _ _) = boxPlus (boxPlus a p) b

boxPlusReduce a@(BoxProd _ _) p = boxPlus a p
boxPlusReduce (BoxPlus a as) p = boxPlus as $ boxPlus a p

safeBoxPlus :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Maybe (BoxProduct a b)
safeBoxPlus a@(BoxProd a1 a2) b@(BoxProd b1 b2)
    | a `boxLess` b = Just b
    | b `boxLess` a = Just a
    | a1 == b1 = if a2 `isOrthogonal` b2 then Just (a1 <> (a2 \./ b2)) else Nothing
    | a2 == b2 = if a1 `isOrthogonal` b1 then Just ((a1 \./ b1) <> a2) else Nothing
    | (a1 /= b1) && (a2 `isOrthogonal` b2) = Just $ BoxPlus a b
    | (a2 /= b2) && (a1 `isOrthogonal` b1) = Just $ BoxPlus a b
    | otherwise = Nothing
safeBoxPlus a@(BoxProd _ _) p@(BoxPlus b rest) = (safeBoxPlus a b) >>= (`safeBoxPlusReduce` rest)
safeBoxPlus p@(BoxPlus _ _) a@(BoxProd _ _) = safeBoxPlus a p
safeBoxPlus (BoxPlus a b) p@(BoxPlus _ _) = (safeBoxPlus a p) >>= (`safeBoxPlus` b)

safeBoxPlusReduce :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Maybe (BoxProduct a b)
safeBoxPlusReduce a@(BoxProd _ _) p = safeBoxPlus a p
safeBoxPlusReduce (BoxPlus a as) p = (safeBoxPlus as) =<< (safeBoxPlus a p)

(<+>) :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> BoxProduct a b
(<+>) = boxPlus

boxNormalForm :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b
boxNormalForm p 
    | length terms > 0 = foldl1 (BoxPlus) terms
    | otherwise = BoxProd zero zero
    where
        terms = filter dropZero $ boxPlusToList p
        dropZero (BoxProd a b)
            | a == zero && b == zero = False
            | otherwise = True

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
allBoxProducts = [boxProd a b | a <- elements, b <- elements]

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
        ortho q = head $ filter (\p -> (isBoxOrthogonal q p) && (p `safeBoxPlus` q == Just one)) elements

isBoxOrthogonal :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Bool
isBoxOrthogonal (BoxProd a1 a2) (BoxProd b1 b2) = (a1 `isOrthogonal` b1) || (a2 `isOrthogonal` b2)
isBoxOrthogonal a@(BoxProd _ _) (BoxPlus c d) = (a `isBoxOrthogonal` c) && (a `isBoxOrthogonal` d)
isBoxOrthogonal p@(BoxPlus _ _) a@(BoxProd _ _) = a `isBoxOrthogonal` p
isBoxOrthogonal (BoxPlus a b) p@(BoxPlus _ _) = (a `isBoxOrthogonal` p) && (b `isBoxOrthogonal` p)

(<->) :: (Logic a) => a -> a -> a
b <-> a = fromJust $ b /\ ortho a
