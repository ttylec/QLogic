{-# LANGUAGE GADTs, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Data.QLogic (QLogic, fromPoset, quotientQLogic, packQLogic, unpackQLogic
                   , infIn, unsafeInfIn
                   , supIn, unsafeSupIn
                   , ocmplIn, zeroOf, oneOf
                   , atomsOf
                   , checkLogic, checkLattice, checkBoolean
                   , checkOrderReverse, checkOrthoIdempotence
                   , checkSupremum, checkOrthomodular, checkSups
                   , checkDistributive
                   ) where

import Data.Maybe
import Control.Monad
import Control.Parallel.Strategies

import Data.Poset
import Data.Poset.Internals

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
data QLogic a where
        QLogic :: Poset a -> (a -> a) -> a -> a -> QLogic a

instance (Show a) => Show (QLogic a) where
        show ql@(QLogic poset _ _ _) = show poset ++
            "\nOrthocompletions:\n" ++ (unlines $ olists) ++
            "\nmin: " ++ show (zeroOf ql) ++
            "\nmax: " ++ show (oneOf ql)
            where
                olists = map (\a -> (show a) ++ "C = " ++ (show $ omap a)) $ elementsOf ql
                omap = ocmplIn ql

instance POrdStruct (QLogic a) a where
        elementsOf (QLogic poset _ _ _) = elementsOf poset
        lessIn (QLogic poset _ _ _) = lessIn poset

-- = Construction
-- |Constructs Poset from the list of POrd data
fromPoset :: (Eq a) => Poset a -> (a -> a) -> QLogic a
fromPoset poset omap = QLogic poset omap min max
    where
        els = elementsOf poset
        min = head $ minimalIn poset els
        max = head $ maximalIn poset els

-- |Convert Poset to packed representation: elements are replaced
-- by sequence of integers and relation is coverted to array representation.
packQLogic :: (Ord a) => QLogic a -> QLogic Int
packQLogic (QLogic poset omap min max) = QLogic pposet pomap pmin pmax
    where
        (packed, pposet) = packPoset' poset
        pomap = packFunc packed omap
        pmin = toKey packed min
        pmax = toKey packed max

-- |Convert packed Poset representation to explicit one.
-- It is not safe: packed must have appropriate length.
unpackQLogic :: (Ord a) => Packed a -> QLogic Int -> QLogic a
unpackQLogic packed (QLogic poset omap min max) = QLogic uposet uomap umin umax
    where
        uposet = unpackPoset packed poset
        uomap = unpackFunc packed omap
        umin = fromKey packed min
        umax = fromKey packed max

-- |Preorder is a partial order relation that is anti-symmetric
-- and transitive but not reflexive. Having a set with a preorder
-- we can construct partialy ordered set by taking the quotient:
quotientQLogic :: (Ord a) => QLogic a -> QLogic (Equiv a)
quotientQLogic (QLogic preposet omap min max) = QLogic poset equivOcmpl equivMin equivMax
    where
        poset = quotientPoset preposet
        els = elementsOf poset
        equivOcmpl = liftFunc els $ omap
        equivMax = fromJust $ equivLookup els max
        equivMin = fromJust $ equivLookup els min

-- = Basic properties
-- | Orthocompletion in quantum logic
ocmplIn :: QLogic a -> (a -> a)
ocmplIn (QLogic _ omap _ _) = omap

-- | The least element of quantum logic
zeroOf :: QLogic a -> a
zeroOf (QLogic  _ _ z _) = z

-- | The greatest element of quantum logic
oneOf :: QLogic a -> a
oneOf (QLogic  _ _ _ o) = o

-- | Checks if elements are disjoint, i.e. if a ≤ ortho b
disjointIn :: QLogic a -> a -> a -> Bool
disjointIn ql a b = lessIn ql a $ ocmplIn ql b

-- | List of atoms in logic,
-- i.e. elements covering zero.
atomsOf :: QLogic a -> [a]
atomsOf ql = minimalIn ql $ filter (not . (`equal` zero)) $ elementsOf ql
    where
        equal = equalIn ql
        zero = zeroOf ql

-- = Axioms

-- |Checks if given structure is a quantum logic
checkLogic :: (Eq a) => QLogic a -> Bool
checkLogic set = and [checkOrderReverse set, 
                     checkOrthoIdempotence set, 
                     checkSupremum set, 
                     checkOrthomodular set]

-- |Checks L2 axiom of logic.
checkOrderReverse :: QLogic a -> Bool
checkOrderReverse ql = and cond
    where
        cond = map id [ocmpl q ≤ ocmpl p | p <- elementsOf ql, 
                                           q <- elementsOf ql, 
                                           p ≤ q] `using` parList rdeepseq
        (≤) = lessIn ql
        ocmpl = ocmplIn ql
        
-- |Check L3 axiom in given set of elements
checkOrthoIdempotence :: (Eq a) => QLogic a -> Bool
checkOrthoIdempotence ql = and cond 
    where
        cond = map idem (elementsOf ql) `using` parList rdeepseq 
        idem p = p == (ocmpl . ocmpl $ p)
        ocmpl = ocmplIn ql

-- |Checks L4 axiom in given set of elements
checkSupremum :: (Eq a) => QLogic a -> Bool
checkSupremum ql = all (/= Nothing) orthosups
    where
        orthosups = map orthosup (elementsOf ql) `using` parList rseq
        orthosup p = foldM sup p [q | q <- elementsOf ql, disjointIn ql p q]
        sup = supIn ql

-- |Checks L5 axiom in given set of elements
checkOrthomodular :: (Eq a) => QLogic a -> Bool
checkOrthomodular ql = and cond
    where
        cond = map id [Just b == rhs a b | a <- set, b <- set, a .<. b] `using` parList rdeepseq
        rhs a b = (inf b $ ocmpl a) >>= (sup a)
        (.<.) = lessIn ql
        inf = infIn ql
        sup = supIn ql
        ocmpl = ocmplIn ql
        set = elementsOf ql

-- |Checks if all sups and infs exist
checkSups :: (Eq a) => QLogic a -> Bool
checkSups ql = sups && infs
    where
        sups = all (/= Nothing) [supIn ql a b | a <- els, b <- els] -- `using` parList rdeepseq
        infs = all (/= Nothing) [infIn ql a b | a <- els, b <- els] -- `using` parList rdeepseq
        els = elementsOf ql

-- |Checks if the logic is orthomodular lattice
checkLattice :: (Eq a) => QLogic a -> Bool
checkLattice ql = checkLogic ql && checkSups ql

-- |Checks distributivity law
-- Remark: it doesn't check if sups and infs exists but assumes
-- existence. Do checkSups before for safe behaviour. 
checkDistributive :: (Eq a) => QLogic a -> Bool
checkDistributive ql = distrib
    where
        distrib = and [(a \/ b) /\ c == (a /\ c) \/ (b /\ c) | a <- els, b <- els, c <- els]
        els = elementsOf ql
        (\/) = unsafeSupIn ql
        (/\) = unsafeInfIn ql

-- = Compatibility
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
compatibleIn :: (Eq a) => QLogic a -> a -> a -> Bool
compatibleIn ql a b = all (fromMaybe False) [are_ortho, are_equal_a, are_equal_b]
    where 
          are_equal_a = liftM2 (==) aa $ Just a
          are_equal_b = liftM2 (==) bb $ Just b
          are_ortho = liftM (mutuallyDisjointIn ql) $ sequence [a1, b1, c] 
          c = infIn ql a b
          a1 = infIn ql a $ ocmplIn ql b
          b1 = infIn ql b $ ocmplIn ql a
          aa = join $ liftM2 (supIn ql) a1 c
          bb = join $ liftM2 (supIn ql) b1 c

-- | Checks is given logic is a Boolean logic
checkBoolean :: (Eq a) => QLogic a -> Bool
checkBoolean ql = mutuallyCompatibleIn ql $ elementsOf ql

-- |Checks if elemensts in given subset are mutually disjoint in set
mutuallyDisjointIn :: (Eq a) => QLogic a -> [a] -> Bool
mutuallyDisjointIn ql = mutuallyBy (disjointIn ql)

-- |Checks if elements in given subset are mutually orthogonal in set
mutuallyCompatibleIn :: (Eq a) => QLogic a -> [a] -> Bool
mutuallyCompatibleIn ql = mutuallyBy (compatibleIn ql)

-- |Utitlity function to perform "mutuall" tests.
mutuallyBy :: (a -> a -> Bool) -> [a] -> Bool
mutuallyBy _ [] = True
mutuallyBy f (a:as) = (all (f a) as) && mutuallyBy f as
