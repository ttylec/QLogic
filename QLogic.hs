{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
module QLogic where

import Control.Monad
import Data.List
import Data.Maybe

import Data.Typeable
import Data.Dynamic
import qualified Data.Map.Lazy as Map

-- |Almost like show, but produce valid Haskell type constructors
-- Used by function that creates static Logic data types.
class (Show a) => Repr a where
        repr :: a -> String
        repr = show

-- |Type class for finite element collection
class Finite a where
        elements :: [a]

-- |Type class of partially ordered type
class (Eq a) => Poset a where
        (.<.) :: a -> a -> Bool
        (.>.) :: a -> a -> Bool

        (.>.) p q = q .<. p

-- |Type class of type that is a quantum logic,
--  i.e. a set L = (Poset a) => [a] with a map
--
--      ortho :: a -> a
--
--  satisfying following axioms:
--
--  (L1) there exists least and greatest (distinct) elements in L
--  (L2) a <= b implies ortho a >= ortho b
--  (L3) ortho . ortho = id
--  (L4) for any countable family [a_1 ..] of elements of L,
--       such that a_i <= ortho a_j for i /= j, supremum
--       of [a_1 ..] is an element of L.
--  (L5) orthomodular law: a <= b implies b = a \/ (b /\ ortho a)
--       (we implicitly assume that the above exists)
class (Poset a) => Logic a where
        one :: a    -- L1 axiom
        zero :: a   -- L1 axiom
        ortho :: a -> a

class (Logic a) => AtomicLogic a where
        atoms :: [a]

class (AtomicLogic a, Finite a) => FiniteLogic a where
        orthoIn :: [a] -> a -> a
        orthoIn _ = ortho

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

instance (FiniteLogic a, FiniteLogic b) => Finite (ZeroOnePasting a b) where
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
-- Functions related to poset structure
--

-- |Calculates join of two elements in given set.
-- If join doesn't exists, returns Nothing.
joinIn :: (Logic a) => [a] -> a -> a -> Maybe a
joinIn set p q
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
        lub = lowestUpperBoundIn set p q

-- |Calculates meet of two elements in given set.
-- If meet doesn't exists, returns Nothing.
meetIn :: (Logic a) => [a] -> a -> a -> Maybe a
meetIn set p q 
    | length glb == 1 = Just $ head glb
    | otherwise = Nothing
    where
        glb = greatestLowerBoundIn set p q

-- |Returns lowest upper bound of two elements
-- in given subset. Note that there might be more
-- than one lowest upper bound.
lowestUpperBoundIn :: (Logic a) => [a] -> a -> a -> [a]
lowestUpperBoundIn set p q = minimal $ filter (\r -> (p .<. r) && (q .<. r)) $ set 

-- |Returns lowest upper bound of two elements
-- in finite logic. Note that giving elements of
-- the whole finite logic to lowestUpperBoundIn
-- is more efficient.
lowestUpperBound :: (FiniteLogic a) => a -> a -> [a]
lowestUpperBound = lowestUpperBoundIn elements

-- |Returns greatest lower bound of two elements
-- in given subset. Note that there might be more
-- than one lowest upper bound.
greatestLowerBoundIn :: (Logic a) => [a] -> a -> a -> [a]
greatestLowerBoundIn set p q = maximal $ filter (\r -> (r .<. p) && (r .<. q)) $ set

-- |Returns greatest lower bound of two elements
-- in finite logic. Note that giving elements of
-- the whole finite logic to lowestUpperBoundIn
-- is more efficient.
greatestLowerBound :: (FiniteLogic a) => a -> a -> [a]
greatestLowerBound = greatestLowerBoundIn elements

-- |Return list of elements greater than element in given set
greaterThanIn :: (Logic a) => [a] -> a -> [a] 
greaterThanIn set p = filter (p .<. ) $ set

-- |Return list of elements less than element in given set
lessThanIn :: (Logic a) => [a] -> a -> [a] 
lessThanIn set p = filter (.<. p) $ set

-- |Returns list of elements covering given one in set
coversOfIn :: (Logic a) => [a] -> a -> [a]
coversOfIn set a = minimal $ filter (not . (a ==)) $ greaterThanIn set a

-- |Returns list of elements overing given one in finite logic
coversOf :: (FiniteLogic a) => a -> [a]
coversOf = coversOfIn elements

-- |Returns True if two elements have join in given set
hasJoinIn :: (Logic a) => [a] -> a -> a -> Bool
hasJoinIn set p q = isJust $ joinIn set p q

-- |Returns true if given two elements are orthogonal in set
isOrthoIn :: (FiniteLogic a) => [a] -> a -> a -> Bool
isOrthoIn set p q = p .<. (orthoIn set q)

-- |Returns True if given two elements are orthogonal
-- in finite logic. 
(-|-) :: (FiniteLogic a) => a -> a -> Bool
(-|-) = isOrthoIn elements

--
-- Functions testing various structures in set
--

checkLogic :: (FiniteLogic a) => [a] -> Bool
checkLogic set = and [checkOrderReverse set, 
                     checkOrthoIdempotence set, 
                     checkSupremum set, 
                     checkOrthomodular set]

-- |Performs check of (L2) axiom of logic.
checkOrderReverse :: (FiniteLogic a) => [a] -> Bool
checkOrderReverse set = and [(orthoIn set q) .<. (orthoIn set p) | 
                           p <- set, q <- set, p .<. q]

-- |Check L3 axiom in given set of elements
checkOrthoIdempotence :: (FiniteLogic a) => [a] -> Bool
checkOrthoIdempotence set = all idem set
    where
        idem p = p == (ortho . ortho $ p)
        ortho = orthoIn set

-- |Checks L4 axiom in given set of elements
checkSupremum :: (FiniteLogic a) => [a] -> Bool
checkSupremum set = all ((/= Nothing) . orthosup) set
    where
        orthosup p = foldM (joinIn set) p [q | q <- set, isOrthoIn set p q]

-- |Checks L5 axiom in given set of elements
checkOrthomodular :: (FiniteLogic a) => [a] -> Bool
checkOrthomodular set = and [(Just b) == (rhs a b) | a <- set, b <- set, a .<. b]
    where
        rhs a b = (meetIn set b $ ortho a) >>= (joinIn set a)

-- |Checks if two elements are compatible in given subset,
-- i.e. a and b are compatible in K whenever there exists
-- a_1, b_1 and c such that:
--
--      a = a1 \/ c     (1a)
--      b = a2 \/ c     (1b)
--
--  and [a1, a2, c] is mutually orthogonal collection
--  of elements.
--
--  Implementation is basically the proof of Proposition 1.3.5 of [1]:
--
--  Assume that a b are compatible. Then: 
--
--      a1 = a /\ ortho b, b1 = b /\ ortho a, c = a /\ b
--
--  (all exists by the assumption), and a1, b1, c are mutually
--  orthogonal, and satisfy (1a) and (1b). If any of these steps fail
--  we showed that a and b cannot be compatible (by contradiction).
--
--  Contrary, if above procedure resulted in True, we showed explicitly
--  that a and b are compatible.
isCompatibleIn :: (FiniteLogic a) => [a] -> a -> a -> Bool
isCompatibleIn set a b = all (fromMaybe False) [are_ortho, are_equal_a, are_equal_b]
    where 
          are_equal_a = liftM2 (==) aa $ Just a
          are_equal_b = liftM2 (==) bb $ Just b
          are_ortho = liftM (mutuallyOrthogonalIn set) $ sequence [a1, b1, c] 
          c = meetIn set a b
          a1 = meetIn set a $ orthoIn set b
          b1 = meetIn set b $ orthoIn set a
          aa = join $ liftM2 (joinIn set) a1 c
          bb = join $ liftM2 (joinIn set) b1 c

-- |Checks if elemensts in given subset are mutually orthogonal in set
mutuallyOrthogonalIn :: (FiniteLogic a) => [a] -> [a] -> Bool
mutuallyOrthogonalIn set = mutuallyBy (isOrthoIn set)

mutuallyOrthogonal :: (FiniteLogic a) => [a] -> Bool
mutuallyOrthogonal = mutuallyOrthogonalIn elements

-- |Checks if elements in given subset are mutually orthogonal in set
mutuallyCompatibleIn :: (FiniteLogic a) => [a] -> [a] -> Bool
mutuallyCompatibleIn set = mutuallyBy (isCompatibleIn set)

-- |Utitlity function to perform "mutuall" tests.
mutuallyBy :: (a -> a -> Bool) -> [a] -> Bool
mutuallyBy _ [] = True
mutuallyBy f (a:as) = (all (f a) as) && mutuallyBy f as

-- isRegular :: (Logic a) => [a] -> Bool
-- isRegular [] = True
-- isRegular (a:as) = 
--
-- debugOrthomodular ps = filter (\ (x, y, z) -> Just y /= z) $ [(a, b, (rhs a b)) | a <- ps, b <- ps, a .<. b]
--     where
--         rhs a b = (b /\ ortho a) >>= (a \/) 

-- |Unsafe join, use only when sure that join exists
unsafeJoinIn :: (Logic a) => [a] -> a -> a -> a
unsafeJoinIn el a b = fromJust $ joinIn el a b

(\./) :: (FiniteLogic a) => a -> a -> a
(\./) = unsafeJoinIn elements

-- |Unsafe meet, use only when sure that meet exists
unsafeMeetIn :: (Logic a) => [a] -> a -> a -> a
unsafeMeetIn el a b = fromJust $ meetIn el a b

(/.\) :: (FiniteLogic a) => a -> a -> a
(/.\) = unsafeMeetIn elements

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
