{-# LANGUAGE GADTs, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Data.QLogic (QLogic(QLogic), fromPOrdStruct, quotientQLogic, packQLogic, unpackQLogic
                   -- , zeroOnePaste
                   , module Data.Poset
                   , atomsOf
                   , atomicDecomposition, decomposition
                   , mutuallyDisjointIn, mutuallyCompatibleIn
                   , isLogic, isLattice, isBoolean
                   , isOrderReverse, isOrthoIdempotence
                   , isSupremum, isOrthomodular, isAllSups
                   , isDistributive, isAtomistic, isAtomistic'
                   , QLogicStruct, ocmplIn, zeroOf, oneOf, orthoIn, compatIn, subLogic 
                   ) where

import Data.List
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Parallel.Strategies

import Data.Poset
import Data.Poset.ConcretePoset
import Data.QLogic.Utils

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- | Quantum logic 
--
-- Quantum logic is defined as a set with partial order
-- relation ≤ and orthocompletion ortho such that:
--
--  (L1) there exists least and greatest (distinct) elements in L
--  (L2) a <= b implies ortho a >= ortho b
--  (L3) ortho . ortho = id
--  (L4) for any countable family [a_1 ..] of elements of L,
--       such that a_i ≤ ortho a_j for i /= j, supremum
--       of [a_1 ..] is an element of L.
--  (L5) orthomodular law: a ≤ b implies b = a \/ (b /\ ortho a)
--       (we implicitly assume that the above exists)
--
-- The constructor of quantum logic does not check these
-- axioms. One need to check them independently. Any function
-- that takes quantum logic as an argument also assumes that
-- the above axioms are satisfied.
--
-- > QLogic poset ortho min max
data QLogic p a where
        QLogic :: (Eq a, POrdStruct p a) => p -> (a -> a) -> a -> a -> QLogic p a

instance (Show p, Show a) => Show (QLogic p a) where
        show ql@(QLogic poset _ _ _) = show poset ++
            "\nOrthocompletions:\n" ++ (unlines $ olists) ++
            "\nmin: " ++ show (zeroOf ql) ++
            "\nmax: " ++ show (oneOf ql)
            where
                olists = map (\a -> (show a) ++ "C = " ++ (show $ omap a)) $ elementsOf ql
                omap = ocmplIn ql

instance POrdStruct (QLogic p a) a where
        elementsOf (QLogic poset _ _ _) = elementsOf poset
        lessIn (QLogic poset _ _ _) = lessIn poset
        supIn (QLogic poset _ _ _) = supIn poset

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
class POrdStruct p a => QLogicStruct p a | p -> a where
    ocmplIn  :: p -> a -> a
    orthoIn  :: p -> a -> a -> Bool
    orthoIn ql a b = lessIn ql a (ocmplIn ql b)
    compatIn :: p -> a -> a -> Bool
    compatIn ql a b = all (fromMaybe False) [are_ortho, are_equal_a, are_equal_b]
        where 
            are_equal_a = liftM2 (equalIn ql) aa $ Just a
            are_equal_b = liftM2 (equalIn ql) bb $ Just b
            are_ortho = liftM (mutuallyBy $ orthoIn ql) $ sequence [a1, b1, c] 
            c = infIn ql a b
            a1 = infIn ql a $ ocmplIn ql b
            b1 = infIn ql b $ ocmplIn ql a
            aa = join $ liftM2 (supIn ql) a1 c
            bb = join $ liftM2 (supIn ql) b1 c
    zeroOf   :: p -> a
    oneOf    :: p -> a
    subLogic :: p -> [a] -> p

instance POrdStruct p a => QLogicStruct (QLogic p a) a where
    ocmplIn (QLogic _ ocmpl _ _) = ocmpl
    zeroOf (QLogic  _ _ z _) = z
    oneOf (QLogic  _ _ _ o) = o
    subLogic ql set = undefined

-- |Constructs Poset from the list of POrd data
fromPOrdStruct :: (Eq a, POrdStruct p a) => p -> (a -> a) -> QLogic p a
fromPOrdStruct poset omap = QLogic poset omap min max
    where
        els = elementsOf poset
        min = head $ minimalIn poset els
        max = head $ maximalIn poset els

-- |Convert Poset to packed representation: elements are replaced
-- by sequence of integers and relation is coverted to array representation.
packQLogic :: (Ord a) => QLogic (Poset a) a -> QLogic (Poset Int) Int
packQLogic (QLogic poset omap min max) = QLogic pposet pomap pmin pmax
    where
        (packed, pposet) = packPoset' poset
        pomap = packFunc packed omap
        pmin = toKey packed min
        pmax = toKey packed max

-- |Convert packed Poset representation to explicit one.
-- It is not safe: packed must have appropriate length.
unpackQLogic :: (Ord a) => Packed a -> QLogic (Poset Int) Int -> QLogic (Poset a) a
unpackQLogic packed (QLogic poset omap min max) = QLogic uposet uomap umin umax
    where
        uposet = unpackPoset packed poset
        uomap = unpackFunc packed omap
        umin = fromKey packed min
        umax = fromKey packed max

-- |Preorder is a partial order relation that is anti-symmetric
-- and transitive but not reflexive. Having a set with a preorder
-- we can construct partialy ordered set by taking the quotient:
quotientQLogic :: (Ord a, POrdStruct p a) => QLogic p a -> QLogic (Poset (Equiv a)) (Equiv a)
quotientQLogic (QLogic preposet omap min max) = QLogic poset equivOcmpl equivMin equivMax
    where
        poset = quotientPoset preposet
        els = elementsOf poset
        equivOcmpl = liftFunc els $ omap
        equivMax = fromJust $ equivLookup els max
        equivMin = fromJust $ equivLookup els min

-- | List of atoms in logic,
-- i.e. elements covering zero.
atomsOf :: QLogicStruct p a => p -> [a]
atomsOf ql = minimalIn ql $ filter (not . (`equal` zero)) $ elementsOf ql
    where
        equal = equalIn ql
        zero = zeroOf ql

-- = Axioms

-- |Checks if given structure is a quantum logic
isLogic :: QLogicStruct p a => p -> Bool
isLogic set = and [isOrderReverse set, 
                     isOrthoIdempotence set, 
                     isSupremum set, 
                     isOrthomodular set]

-- |Checks L2 axiom of logic.
isOrderReverse :: QLogicStruct p a => p -> Bool
isOrderReverse ql = and cond
    where
        cond = map id [ocmpl q ≤ ocmpl p | p <- elementsOf ql, 
                                           q <- elementsOf ql, 
                                           p ≤ q] `using` parList rdeepseq
        (≤) = lessIn ql
        ocmpl = ocmplIn ql
        
-- |Check L3 axiom in given set of elements
isOrthoIdempotence :: QLogicStruct p a => p -> Bool
isOrthoIdempotence ql = and cond 
    where
        cond = map idem (elementsOf ql) `using` parList rdeepseq 
        idem p = equalIn ql p (ocmpl . ocmpl $ p)
        ocmpl = ocmplIn ql

-- |Checks L4 axiom in given set of elements
isSupremum :: QLogicStruct p a => p -> Bool
isSupremum ql = all isJust [supIn ql p q | p <- elementsOf ql,
                                              q <- elementsOf ql,
                                              orthoIn ql p q]

-- |Checks L5 axiom in given set of elements
isOrthomodular :: QLogicStruct p a => p -> Bool
isOrthomodular ql = and cond
    where
        cond = map id [b === rhs a b | a <- set, b <- set, a .<. b] `using` parList rdeepseq
        rhs a b = (b /\ ocmpl a) \/ a
        (.<.) = lessIn ql
        (\/) = unsafeSupIn ql
        (/\) = unsafeInfIn ql
        (===) = equalIn ql
        ocmpl = ocmplIn ql
        set = elementsOf ql

-- isOrthomodular' :: QLogicStruct p a => p -> [(a, a)]
isOrthomodular' ql = lefts [orthomodularPair ql a b | a <- set, b <- set, a .<. b]
    where
        (.<.) = lessIn ql
        set = elementsOf ql

orthomodularPair ql a b 
    | not $ a .<. b   = Right True
    | otherwise = if b === rhs then Right True else Left ((a, b), rhs)
        where
            rhs = (b /\ ocmpl a) \/ a
            (.<.) = lessIn ql
            (\/) = unsafeSupIn ql
            (/\) = unsafeInfIn ql
            (===) = equalIn ql
            ocmpl = ocmplIn ql

orthomodularRHS ql a b =  (b /\ ocmpl a) \/ a
    where
            (.<.) = lessIn ql
            (\/) = unsafeSupIn ql
            (/\) = unsafeInfIn ql
            (===) = equalIn ql
            ocmpl = ocmplIn ql


-- |Checks if all sups and infs exist
isAllSups :: QLogicStruct p a => p -> Bool
isAllSups ql = sups && infs
    where
        sups = all isJust [supIn ql a b | a <- els, b <- els] -- `using` parList rdeepseq
        infs = all isJust [infIn ql a b | a <- els, b <- els] -- `using` parList rdeepseq
        els = elementsOf ql

-- |Checks if the logic is orthomodular lattice
isLattice :: QLogicStruct p a => p -> Bool
isLattice ql = isLogic ql && isAllSups ql

-- |Checks distributivity law
-- Remark: it doesn't check if sups and infs exists but assumes
-- existence. Do checkSups before for safe behaviour. 
isDistributive :: QLogicStruct p a => p -> Bool
isDistributive ql = distrib
    where
        distrib = and [((a \/ b) /\ c) === ((a /\ c) \/ (b /\ c)) | a <- els, b <- els, c <- els]
        els = elementsOf ql
        (\/) = unsafeSupIn ql
        (/\) = unsafeInfIn ql
        (===) = equalIn ql

isAtomistic' :: QLogicStruct p a => p -> Either a Bool
isAtomistic' ql = case find (not . check) $ elementsOf ql of
    Just q  -> Left q
    Nothing -> Right True
    where
        check q = case sup (atomsLessThan q) of
            Just p  -> (p ≤ q) && (q ≤ p)
            Nothing -> False
        sup []     = Just $ zeroOf ql
        sup (a:[]) = Just a
        sup (a:as) = foldM (supIn ql) a as
        atomsLessThan q = filter (≤ q) atoms
        atoms = atomsOf ql
        (≤) = lessIn ql

-- | Checks if the logic is atomistic.
-- It assumes that it is atomic.
isAtomistic :: QLogicStruct p a => p -> Bool
isAtomistic ql = all check $ elementsOf ql
    where
        check q = case lubIn ql (atomsLessThan q) of
            (p:[])    -> (p ≤ q) && (q ≤ p)
            otherwise -> False
        atomsLessThan q = filter (≤ q) atoms
        atoms = atomsOf ql
        (≤) = lessIn ql

-- | Checks is given logic is a Boolean logic
isBoolean :: QLogicStruct p a => p -> Bool
isBoolean ql = mutuallyCompatibleIn ql $ elementsOf ql

-- | Checks if elemensts in given subset are mutually disjoint in set
mutuallyDisjointIn :: QLogicStruct p a => p -> [a] -> Bool
mutuallyDisjointIn ql = mutuallyBy (orthoIn ql)

-- | Checks if elements in given subset are mutually orthogonal in set
mutuallyCompatibleIn :: QLogicStruct p a => p -> [a] -> Bool
mutuallyCompatibleIn ql = mutuallyBy (compatIn ql)

atomicDecomposition :: (QLogicStruct p a) => p -> a -> [[a]]
atomicDecomposition ql a = filter (sumUpTo a) . subsetsBy (orthoIn ql) . atomsOf $ ql 
    where
        sumUpTo a [] = False
        sumUpTo a (b:bs) = foldl' (\/) b bs === a
        (\/) = unsafeSupIn ql
        (===) = equalIn ql

decomposition :: (QLogicStruct p a) => p -> [a] -> a -> [[a]]
decomposition ql as q = filter (sumUpTo q) . subsetsBy (orthoIn ql) $ as
    where
        sumUpTo a [] = False
        sumUpTo a (b:bs) = foldl' (\/) b bs === a
        (\/) = unsafeSupIn ql
        (===) = equalIn ql
