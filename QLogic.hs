{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
module QLogic where

import Control.Monad
import Data.List
import Data.Maybe

import Data.Typeable
import qualified Data.Map.Lazy as Map

class Repr a where
        repr :: a -> String

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
        orthoIn :: [a] -> a -> a
        
        ortho = orthoIn elements
        orthoIn _ = ortho
        -- (\/) :: a -> a -> Maybe a
        -- (/\) :: a -> a -> Maybe a

        -- p \/ q
        --     | length lub == 1 = Just $ head lub
        --     | otherwise = Nothing
        --     where
        --         lub = lowestUpperBound p q

        -- p /\ q
        --     | length glb == 1 = Just $ head glb
        --     | otherwise = Nothing
        --     where
        --         glb = greatestLowerBound p q
        
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

class (Logic a) => AtomicLogic a where
        atoms :: [a]
--
-- Lattice functions
--

joinIn :: (Logic a) => [a] -> a -> a -> Maybe a
joinIn set p q
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
        lub = lowestUpperBoundIn set p q

meetIn :: (Logic a) => [a] -> a -> a -> Maybe a
meetIn set p q 
    | length glb == 1 = Just $ head glb
    | otherwise = Nothing
    where
        glb = greatestLowerBoundIn set p q
        
(\/) :: (Logic a) => a -> a -> Maybe a
(\/) = joinIn elements

(/\) :: (Logic a) => a -> a -> Maybe a
(/\) = meetIn elements

lowestUpperBoundIn :: (Logic a) => [a] -> a -> a -> [a]
lowestUpperBoundIn set p q = minimal $ filter (\r -> (p .<. r) && (q .<. r)) $ set 

lowestUpperBound :: (Logic a) => a -> a -> [a]
lowestUpperBound = lowestUpperBoundIn elements

greatestLowerBoundIn :: (Logic a) => [a] -> a -> a -> [a]
greatestLowerBoundIn set p q = maximal $ filter (\r -> (r .<. p) && (r .<. q)) $ set

greatestLowerBound :: (Logic a) => a -> a -> [a]
greatestLowerBound = greatestLowerBoundIn elements

-- |Return list of elements in Logic a that are greater than given element
greaterThanIn :: (Logic a) => [a] -> a -> [a] 
greaterThanIn set p = filter (p .<. ) $ set

greaterThan :: (Logic a) => a -> [a] 
greaterThan = greaterThanIn elements

-- |Return list of elements in Logic a that are less than given element
lessThanIn :: (Logic a) => [a] -> a -> [a] 
lessThanIn set p = filter (.<. p) $ set

lessThan :: (Logic a) => a -> [a] 
lessThan = lessThanIn elements

-- |Returns list of elements covering given one
coversOfIn :: (Logic a) => [a] -> a -> [a]
coversOfIn set a = minimal $ filter (not . (a ==)) $ greaterThanIn set a

coversOf :: (Logic a) => a -> [a]
coversOf = coversOfIn elements

-- |Returns True if two elements have join
hasJoinIn :: (Logic a) => [a] -> a -> a -> Bool
hasJoinIn set p q = isJust $ joinIn set p q

hasJoin :: (Logic a) => a -> a -> Bool
hasJoin = hasJoinIn elements
 
checkOrderReverse :: (Logic a) => [a] -> Bool
checkOrderReverse ps = all id [(orthoIn ps q) .<. (orthoIn ps p) | 
                              p <- ps, q <- ps, p .<. q]

isOrthoIn :: (Logic a) => [a] -> a -> a -> Bool
isOrthoIn set p q = p .<. (orthoIn set q)

(-|-) :: (Logic a) => a -> a -> Bool
(-|-) = isOrthoIn elements

isCompatibleIn :: (Logic a) => [a] -> a -> a -> Bool
isCompatibleIn set a b = all (id) $ map (fromMaybe False) [are_ortho, are_equal_a, are_equal_b]
    where 
          are_equal_a = liftM2 (==) aa $ Just a
          are_equal_b = liftM2 (==) bb $ Just b
          are_ortho = liftM (mutuallyOrthogonalIn set) $ sequence [a1, b1, c] 
          c = a /\ b
          a1 = a /\ orth b
          b1 = b /\ orth a
          aa = join $ liftM2 (\/) a1 c
          bb = join $ liftM2 (\/) b1 c

          (/\) = meetIn set
          (\/) = joinIn set
          orth = orthoIn set 

mutuallyOrthogonalIn :: (Logic a) => [a] -> [a] -> Bool
mutuallyOrthogonalIn set = mutuallyBy (isOrthoIn set)

mutuallyOrthogonal :: (Logic a) => [a] -> Bool
mutuallyOrthogonal = mutuallyOrthogonalIn elements

mutuallyCompatibleIn :: (Logic a) => [a] -> [a] -> Bool
mutuallyCompatibleIn set = mutuallyBy (isCompatibleIn set)

mutuallyBy :: (a -> a -> Bool) -> [a] -> Bool
mutuallyBy _ [] = True
mutuallyBy f (a:as) = (all (f a) as) && mutuallyBy f as

-- isRegular :: (Logic a) => [a] -> Bool
-- isRegular [] = True
-- isRegular (a:as) = 

checkSupremum :: (Logic a) => [a] -> Bool
checkSupremum ps = all ((/= Nothing) . orthosup) ps
    where
        orthosup p = foldM (\/) p [q | q <- ps, p -|- q]

checkOrthomodular :: (Logic a) => [a] -> Bool
checkOrthomodular ps = all id [(Just b) == (rhs a b) | a <- ps, b <- ps, a .<. b]
    where
        rhs a b = (b /\ ortho a) >>= (\/ a) 

debugOrthomodular ps = filter (\ (x, y, z) -> Just y /= z) $ [(a, b, (rhs a b)) | a <- ps, b <- ps, a .<. b]
    where
        rhs a b = (b /\ ortho a) >>= (a \/) 

-- |Unsafe join, use only when sure that join exists
unsafeJoinIn :: (Logic a) => [a] -> a -> a -> a
unsafeJoinIn el a b = fromJust $ joinIn el a b

a \./ b = fromJust $ a \/ b

-- |Unsafe meet, use only when sure that meet exists
unsafeMeetIn :: (Logic a) => [a] -> a -> a -> a
unsafeMeetIn el a b = fromJust $ meetIn el a b
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
