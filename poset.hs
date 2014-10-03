{-# LANGUAGE ExistentialQuantification #-}
module Poset where

class Finite a where
        elements :: [a]

class (Eq a) => Poset a where
        (.<.) :: a -> a -> Bool
        (.>.) :: a -> a -> Bool

        (.>.) p q = q .<. p

class (Finite a, Poset a) => Logic a where
        one :: a
        zero :: a

data SimpleElements = Zero | A | B | One deriving (Enum, Bounded, Show, Eq)
simpleRelation :: SimpleElements -> SimpleElements -> Bool
simpleRelation p q
    | p == q = True
simpleRelation Zero _ = True
simpleRelation _ Zero = False
simpleRelation One _ = False
simpleRelation _ One = True
simpleRelation _ _ = False

instance Finite SimpleElements where
        elements = [minBound..]

instance Poset SimpleElements where
        (.<.) = simpleRelation

instance Logic SimpleElements where
        one = One
        zero = Zero

newtype Space4 = Space4 [Int] deriving (Show, Eq)

instance Finite Space4 where
        elements = map Space4 $ subsets [1, 2, 3, 4] 

instance Poset Space4 where
       (.<.) (Space4 a) (Space4 b) = all (`elem` b) a

instance Logic Space4 where
        one = Space4 [1, 2, 3, 4]
        zero = Space4 []

greaterThan :: (Logic a) => a -> [a] 
greaterThan p = filter (p .<. ) $ elements

lessThan :: (Logic a) => a -> [a] 
lessThan p = filter (.<. p) $ elements

lowestUpperBound :: (Logic a) => a -> a -> [a]
lowestUpperBound p q = minimal $ intersection (greaterThan p) (greaterThan q)

greatestLowerBound :: (Logic a) => a -> a -> [a]
greatestLowerBound p q = maximal $ intersection (lessThan p) (lessThan q)

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

minimal :: (Poset a) => [a] -> [a]
minimal [] = []
minimal [p] = [p]
minimal (p:ps) 
    | any (.<. p) ps = minimal ps
    | otherwise = p:(minimal $ filter (not . (p .<.)) ps)

maximal :: (Poset a) => [a] -> [a]
maximal [] = []
maximal [p] = [p]
maximal (p:ps) 
    | any (p .<.) ps = maximal ps
    | otherwise = p:(maximal $ filter (not . (.<. p)) ps)

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection ps qs = filter (`elem` qs) ps
