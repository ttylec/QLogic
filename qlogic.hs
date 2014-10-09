{-# LANGUAGE ExistentialQuantification #-}
module QLogic where

import Control.Monad
import Data.List
import Data.Maybe

-- |Type class for finite element collection
class Finite a where
        elements :: [a]

-- |Type class of partially ordered type
class (Eq a) => Poset a where
        (.<.) :: a -> a -> Bool
        (.>.) :: a -> a -> Bool

        (.>.) p q = q .<. p

-- |Type class of type that is a quantum logic,
--  i.e. bounded (here finite) poset.
class (Finite a, Poset a) => Logic a where
        one :: a
        zero :: a
        ortho :: a -> a
        (\/) :: a -> a -> Maybe a
        (/\) :: a -> a -> Maybe a

        p \/ q
            | length lub == 1 = Just $ head lub
            | otherwise = Nothing
            where
                lub = lowestUpperBound p q

        p /\ q
            | length glb == 1 = Just $ head glb
            | otherwise = Nothing
            where
                glb = greatestLowerBound p q
        
--
-- Creating new 
--
data Product a b = Product a b deriving (Eq)

instance (Show a, Show b) => Show (Product a b) where
        show (Product a b) = (show a) ++ " ⨉ " ++ (show b)

instance (Finite a, Finite b) => Finite (Product a b) where
        elements = [Product p q | p <- elements, q <- elements]

instance (Poset a, Poset b) => Poset (Product a b) where
        (Product p1 p2) .<. (Product q1 q2) = (p1 .<. q1) && (p2 .<. q2)

instance (Logic a, Logic b) => Logic (Product a b) where
        one = Product one one
        zero = Product zero zero
        ortho (Product a b) = Product (ortho a) (ortho b)

data ZeroOnePasting a b = FirstZOP a | SecondZOP b deriving (Show)

instance (Logic a, Logic b) => Eq (ZeroOnePasting a b) where
        (FirstZOP p) == (SecondZOP q)
            | p == one && q == one = True
            | p == zero && q == zero = True
            | otherwise = False
        a@(SecondZOP p) == b@(FirstZOP q) = b == a
        (FirstZOP p) == (FirstZOP q) = p == q
        (SecondZOP p) == (SecondZOP q) = p == q

instance (Logic a, Logic b) => Finite (ZeroOnePasting a b) where
        elements = [FirstZOP p | p <- elements] ++
                   [SecondZOP p | p <- elements \\ [zero, one]]

instance (Logic a, Logic b) => Poset (ZeroOnePasting a b) where
        (FirstZOP p) .<. (SecondZOP q)
            | p == zero = True
            | p == one && q == one = True
            | otherwise = False
        (SecondZOP p) .<. (FirstZOP q)
            | q == one = True
            | p == zero && q == zero = True
            | otherwise = False
        (FirstZOP p) .<. (FirstZOP q) = p .<. q
        (SecondZOP p) .<. (SecondZOP q) = p .<. q

instance (Logic a, Logic b) => Logic (ZeroOnePasting a b) where
        one = FirstZOP one
        zero = FirstZOP zero 
        ortho (FirstZOP p) = FirstZOP (ortho p)
        ortho (SecondZOP p) = SecondZOP (ortho p)

--
-- Lattice functions
--

-- |Return list of elements in Logic a that are greater than given element
greaterThan :: (Logic a) => a -> [a] 
greaterThan p = filter (p .<. ) $ elements

-- |Return list of elements in Logic a that are less than given element
lessThan :: (Logic a) => a -> [a] 
lessThan p = filter (.<. p) $ elements

lowestUpperBound :: (Logic a) => a -> a -> [a]
lowestUpperBound p q = minimal $ filter (\r -> (p .<. r) && (q .<. r)) $ elements

greatestLowerBound :: (Logic a) => a -> a -> [a]
greatestLowerBound p q = maximal $ filter (\r -> (r .<. p) && (r .<. q)) $ elements

coversOf :: (Logic a) => a -> [a]
coversOf a = minimal $filter (not . (a ==)) $ greaterThan a

hasJoin :: (Logic a) => a -> a -> Bool
hasJoin p q = isJust $ p \/ q
 
checkOrderReverse :: (Logic a) => [a] -> Bool
checkOrderReverse ps = all id [(ortho q) .<. (ortho p) | 
                              p <- ps, q <- ps, p .<. q]

isOrthogonal :: (Logic a) => a -> a -> Bool
isOrthogonal p q = p .<. ortho q

isCompatible :: (Logic a) => a -> a -> Bool
isCompatible a b = all (id) $ map (fromMaybe False) [are_ortho, are_equal_a, are_equal_b]
    where 
          are_equal_a = liftM2 (==) aa $ Just a
          are_equal_b = liftM2 (==) bb $ Just b
          are_ortho = liftM mutuallyOrthogonal $ sequence [a1, b1, c] 
          c = a /\ b
          a1 = a /\ ortho b
          b1 = b /\ ortho a
          aa = join $ liftM2 (\/) a1 c
          bb = join $ liftM2 (\/) b1 c

mutuallyOrthogonal :: (Logic a) => [a] -> Bool
mutuallyOrthogonal = mutuallyBy isOrthogonal

mutuallyCompatible :: (Logic a) => [a] -> Bool
mutuallyCompatible = mutuallyBy isCompatible

mutuallyBy :: (a -> a -> Bool) -> [a] -> Bool
mutuallyBy _ [] = True
mutuallyBy f (a:as) = (all (f a) as) && mutuallyBy f as

-- isRegular :: (Logic a) => [a] -> Bool
-- isRegular [] = True
-- isRegular (a:as) = 

checkSupremum :: (Logic a) => [a] -> Bool
checkSupremum ps = all ((/= Nothing) . orthosup) ps
    where
        orthosup p = foldM (\/) p [q | q <- ps, p `isOrthogonal` q]

checkOrthomodular :: (Logic a) => [a] -> Bool
checkOrthomodular ps = all id [(Just b) == (rhs a b) | a <- ps, b <- ps, a .<. b]
    where
        rhs a b = (b /\ ortho a) >>= (\/ a) 

debugOrthomodular ps = filter (\ (x, y, z) -> Just y /= z) $ [(a, b, (rhs a b)) | a <- ps, b <- ps, a .<. b]
    where
        rhs a b = (b /\ ortho a) >>= (a \/) 

-- |Unsafe join, use only when sure that join exists
a \./ b = fromJust $ a \/ b

-- |Unsafe meet, use only when sure that meet exists
a /.\ b = fromJust $ a /\ b

--
-- Poset functions
--

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

--
-- Auxilliary functions
--
subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)
