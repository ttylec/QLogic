{-# LANGUAGE ExistentialQuantification #-}
module Poset where

import Control.Monad
import Control.Applicative

-- |Type class for finite element collection
class Finite a where
        elements :: [a]

-- |Type class of partially ordered type
class (Eq a) => Poset a where
        (.<.) :: a -> a -> Bool
        (.>.) :: a -> a -> Bool

        (.>.) p q = q .<. p

{- |
  Type class of type that is a quantum logic,
  i.e. bounded (here finite) poset.
-}
class (Finite a, Poset a) => Logic a where
        one :: a
        zero :: a
        ortho :: a -> a

-- |Simple example of quantum logic 
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
        ortho Zero = One
        ortho One = Zero
        ortho A = B
        ortho B = A

-- |Quantum logic of subsets of 4 element set
newtype Space4 = Space4 [Int] deriving (Show, Eq)

instance Finite Space4 where
        elements = map Space4 $ subsets [1, 2, 3, 4] 

instance Poset Space4 where
       (.<.) (Space4 a) (Space4 b) = all (`elem` b) a

instance Logic Space4 where
        one = Space4 [1, 2, 3, 4]
        zero = Space4 []
        ortho (Space4 a) = Space4 $ [1, 2, 3, 4] `setminus` a

data Product a b = Product a b deriving (Eq, Show)

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
                   [SecondZOP p | p <- elements `setminus` [zero, one]]

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
lowestUpperBound p q = minimal $ intersection (greaterThan p) (greaterThan q)

greatestLowerBound :: (Logic a) => a -> a -> [a]
greatestLowerBound p q = maximal $ intersection (lessThan p) (lessThan q)

(\/) :: (Logic a) => a -> a -> Maybe a
p \/ q
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
        lub = lowestUpperBound p q

(/\) :: (Logic a) => a -> a -> Maybe a
p /\ q
    | length glb == 1 = Just $ head glb
    | otherwise = Nothing
    where
        glb = greatestLowerBound p q

hasJoin :: (Logic a) => a -> a -> Bool
hasJoin p q = (p \/ q) /= Nothing
 
checkOrderReverse :: (Logic a) => [a] -> Bool
checkOrderReverse ps = all id [(ortho q) .<. (ortho p) | 
                              p <- ps, q <- ps, p .<. q]

(-|-) :: (Logic a) => a -> a -> Bool
p -|- q = p .<. ortho q

checkSupremum :: (Logic a) => [a] -> Bool
checkSupremum ps = all ((/= Nothing) . orthosup) ps
    where
        orthosup p = foldM (\/) p [q | q <- ps, p -|- q]

checkOrthomodular :: (Logic a) => [a] -> Bool
checkOrthomodular ps = all id [(Just b) == (rhs a b) | a <- ps, b <- ps, a .<. b]
    where
        rhs a b = (b /\ ortho a) >>= (a \/) 

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

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection ps qs = filter (`elem` qs) ps

setminus :: (Eq a) => [a] -> [a] -> [a]
a `setminus` b = filter (not . (`elem` b)) a
