{-# LANGUAGE GADTs, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Data.QLogic (QLogic(QLogic), fromPoset, quotientQLogic, packQLogic, unpackQLogic
                   -- , zeroOnePaste
                   , module Data.Poset
                   -- , disjointIn
                   , ocmplIn, zeroOf, oneOf
                   , atomsOf
                   , atomicDecomposition
                   , mutuallyDisjointIn, mutuallyCompatibleIn
                   , isLogic, isLattice, isBoolean
                   , isOrderReverse, isOrthoIdempotence
                   , isSupremum, isOrthomodular, isSups
                   , isDistributive
                   -- , compatibleIn
                   , CLogic(CLogic)
                   , QLogicStruct, orthoIn, compatIn 
                   ) where

import Data.List
import Data.Maybe
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

class (Eq a, POrdStruct p a) => QLogicStruct p a | p -> a where
    ocmplIn  :: p -> a -> a
    orthoIn  :: p -> a -> a -> Bool
    orthoIn ql a b = lessIn ql a (ocmplIn ql b)
    compatIn :: p -> a -> a -> Bool
    compatIn ql a b = all (fromMaybe False) [are_ortho, are_equal_a, are_equal_b]
        where 
            are_equal_a = liftM2 (==) aa $ Just a
            are_equal_b = liftM2 (==) bb $ Just b
            are_ortho = liftM (mutuallyBy $ orthoIn ql) $ sequence [a1, b1, c] 
            c = infIn ql a b
            a1 = infIn ql a $ ocmplIn ql b
            b1 = infIn ql b $ ocmplIn ql a
            aa = join $ liftM2 (supIn ql) a1 c
            bb = join $ liftM2 (supIn ql) b1 c
    zeroOf   :: p -> a
    oneOf    :: p -> a

instance (Eq a, POrdStruct p a) => QLogicStruct (QLogic p a) a where
    ocmplIn (QLogic _ ocmpl _ _) = ocmpl
    zeroOf (QLogic  _ _ z _) = z
    oneOf (QLogic  _ _ _ o) = o

newtype CLogic = CLogic (QLogic ConcretePosetInt IntSet)

instance POrdStruct CLogic IntSet where
    elementsOf (CLogic ql) = elementsOf ql
    lessIn (CLogic ql) = lessIn ql
    supIn (CLogic ql) = supIn ql

instance QLogicStruct CLogic IntSet where
    ocmplIn (CLogic ql) = ocmplIn ql
    orthoIn ql a b = IntSet.null $ IntSet.intersection a b
    compatIn ql a b = IntSet.intersection a b `elem` elementsOf ql
    zeroOf (CLogic ql) = zeroOf ql
    oneOf (CLogic ql) = oneOf ql

-- = Construction
-- -- |Construct from set of elements in QLgc class.
-- fromQLgc :: (QLgc a) => [a] -> QLogic a
-- fromQLgc els = QLogic (fromPOrd els) qlgcOcmpl min max
--     where
--         min = qlgcMin . head $ els
--         max = qlgcMax . head $ els

-- |Constructs Poset from the list of POrd data
fromPoset :: (Eq a, POrdStruct p a) => p -> (a -> a) -> QLogic p a
fromPoset poset omap = QLogic poset omap min max
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
quotientQLogic :: (Ord a) => QLogic (Poset a) a -> QLogic (Poset (Equiv a)) (Equiv a)
quotientQLogic (QLogic preposet omap min max) = QLogic poset equivOcmpl equivMin equivMax
    where
        poset = quotientPoset preposet
        els = elementsOf poset
        equivOcmpl = liftFunc els $ omap
        equivMax = fromJust $ equivLookup els max
        equivMin = fromJust $ equivLookup els min

-- data ZOP a b = ZOPZero | LeftZOP a | RightZOP b | ZOPOne
-- 
-- instance (Eq a, Eq b) => Eq (ZOP a b) where
--         ZOPZero == ZOPZero = True
--         ZOPOne == ZOPOne = True
--         (LeftZOP a) == (LeftZOP b) = a == b
--         (RightZOP a) == (RightZOP b) = a == b
--         _ == _ = False
-- 
-- instance (Show a, Show b) => Show (ZOP a b) where
--         show ZOPZero = "Zero"
--         show ZOPOne = "One"
--         show (LeftZOP a) = "L" ++ show a
--         show (RightZOP a) = "R" ++ show a
-- 
-- zopLess :: (Ord a, Ord b, POrdStruct qla a, POrdStruct qlb b) => (qla, qlb) -> ZOP a b -> ZOP a b -> Bool
-- zopLess (qla, _) (LeftZOP a) (LeftZOP b) = lessIn qla a b
-- zopLess (_, qlb) (RightZOP a) (RightZOP b) = lessIn qlb a b
-- zopLess _ _ ZOPOne = True
-- zopLess _ ZOPZero _ = True
-- zopLess _ _ _ = False
-- 
-- zopOcmpl :: (Ord a, Ord b) => (QLogic a, QLogic b) -> ZOP a b -> ZOP a b 
-- zopOcmpl (qla, _) (LeftZOP a) = LeftZOP $ ocmplIn qla a
-- zopOcmpl (_, qlb) (RightZOP b) = RightZOP $ ocmplIn qlb b
-- zopOcmpl _ ZOPZero = ZOPOne
-- zopOcmpl _ ZOPOne = ZOPZero
-- 
-- zeroOnePaste :: (Ord a, Ord b) => QLogic a -> QLogic b -> QLogic (ZOP a b)
-- zeroOnePaste qla qlb = QLogic poset (zopOcmpl (qla, qlb)) ZOPZero ZOPOne
--     where
--         lefts = [LeftZOP a | a <- elementsOf qla, a /= zeroOf qla, a /= oneOf qla] 
--         rights = [RightZOP b | b <- elementsOf qlb, b /= zeroOf qlb, b /= oneOf qlb]
--         els = ZOPZero:ZOPOne:(lefts ++ rights)
--         poset = fromFunc els (zopLess (qla, qlb))

-- = Basic properties
-- -- | Orthocompletion in quantum logic
-- ocmplIn :: QLogic p a -> (a -> a)
-- ocmplIn (QLogic _ omap _ _) = omap

-- -- | The least element of quantum logic
-- zeroOf :: QLogic p a -> a
-- zeroOf (QLogic  _ _ z _) = z
-- 
-- -- | The greatest element of quantum logic
-- oneOf :: QLogic p a -> a
-- oneOf (QLogic  _ _ _ o) = o

-- -- | Checks if elements are disjoint, i.e. if a ≤ ortho b
-- disjointIn :: QLogic p a -> a -> a -> Bool
-- disjointIn ql a b = lessIn ql a $ ocmplIn ql b

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
        idem p = p == (ocmpl . ocmpl $ p)
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
        cond = map id [Just b == rhs a b | a <- set, b <- set, a .<. b] `using` parList rdeepseq
        rhs a b = (inf b $ ocmpl a) >>= (sup a)
        (.<.) = lessIn ql
        inf = infIn ql
        sup = supIn ql
        ocmpl = ocmplIn ql
        set = elementsOf ql

-- |Checks if all sups and infs exist
isSups :: QLogicStruct p a => p -> Bool
isSups ql = sups && infs
    where
        sups = all (/= Nothing) [supIn ql a b | a <- els, b <- els] -- `using` parList rdeepseq
        infs = all (/= Nothing) [infIn ql a b | a <- els, b <- els] -- `using` parList rdeepseq
        els = elementsOf ql

-- |Checks if the logic is orthomodular lattice
isLattice :: QLogicStruct p a => p -> Bool
isLattice ql = isLogic ql && isSups ql

-- |Checks distributivity law
-- Remark: it doesn't check if sups and infs exists but assumes
-- existence. Do checkSups before for safe behaviour. 
isDistributive :: QLogicStruct p a => p -> Bool
isDistributive ql = distrib
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
-- compatibleIn :: (Eq a) => QLogic p a -> a -> a -> Bool
-- compatibleIn ql a b = all (fromMaybe False) [are_ortho, are_equal_a, are_equal_b]
--     where 
--           are_equal_a = liftM2 (==) aa $ Just a
--           are_equal_b = liftM2 (==) bb $ Just b
--           are_ortho = liftM (mutuallyDisjointIn ql) $ sequence [a1, b1, c] 
--           c = infIn ql a b
--           a1 = infIn ql a $ ocmplIn ql b
--           b1 = infIn ql b $ ocmplIn ql a
--           aa = join $ liftM2 (supIn ql) a1 c
--           bb = join $ liftM2 (supIn ql) b1 c

-- | Checks is given logic is a Boolean logic
isBoolean :: QLogicStruct p a => p -> Bool
isBoolean ql = mutuallyCompatibleIn ql $ elementsOf ql

-- |Checks if elemensts in given subset are mutually disjoint in set
mutuallyDisjointIn :: QLogicStruct p a => p -> [a] -> Bool
mutuallyDisjointIn ql = mutuallyBy (orthoIn ql)

-- |Checks if elements in given subset are mutually orthogonal in set
mutuallyCompatibleIn :: QLogicStruct p a => p -> [a] -> Bool
mutuallyCompatibleIn ql = mutuallyBy (compatIn ql)

atomicDecomposition :: (Eq a, QLogicStruct p a) => p -> a -> [[a]]
atomicDecomposition ql a = filter (sumUpTo a) . subsetsBy (orthoIn ql) . atomsOf $ ql 
    where
        sumUpTo a [] = False
        sumUpTo a (b:bs) = foldl' (unsafeSupIn ql) b bs == a
