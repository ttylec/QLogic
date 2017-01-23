{- |
Module      :  $Header$
Description :  QLogic class and data type and basic operations
Copyright   :  (c) Tomasz Tylec, CFT PAN
License     :  MIT

Maintainer  :  ttylec@gmail.com
Stability   :  experimental
Portability :  non-portable (MPTC, FD, FI)


Quantum logic L is defined as a set with partial order
relation ≤ and orthocompletion @ortho@ such that:

 (L1) there exists least and greatest (distinct) elements in L
 (L2) @a <= b@ implies @ortho a >= ortho b@
 (L3) @ortho . ortho = id@
 (L4) for any countable family @[a_1 ..]@ of elements of L,
      such that @a_i ≤ ortho a_j@ for @i /= j@, supremum
      of @[a_1 ..]@ is an element of L.
 (L5) orthomodular law: @a ≤ b@ implies @b = a \/ (b /\ ortho a)@
      (we implicitly assume that the above exists)


Two elements @a@, @b@ of a quantum logic are /compatible/
in K whenever there exists @a1@, @b1@ and @c@ such that:

@
     a = a1 \/ c     (1a)
     b = a2 \/ c     (1b)
@

and @[a1, a2, c]@ is a mutually orthogonal collection
of elements.

Bibliography:
[1] P. Ptak and S. Pulmannova, Orthomodular Structures as
    Quantum Logics (Springer, 1991).
-}

{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, BangPatterns #-}

module QLogic (
  -- * Class and basic types
    QLogic(QLogic)
    , QLogicStruct, ocmplIn, zeroOf, oneOf, orthoIn, compatIn
    -- * Construction
    , fromPOrdStruct, quotientQLogic, packQLogic, unpackQLogic
    -- * Atoms
    , atomsOf , atomicDecomposition, decompose
    , decomposeInto
    -- * Properties of the elements
    , mutuallyDisjointIn, mutuallyCompatibleIn
    -- * Properties of the structure
    , isLogic, isLattice, isBoolean
    , isOrderReverse, isOrthoIdempotence
    , isSupremum
    , isOrthomodular
    , isDistributive, isAtomistic
    , generateOML, generateOMP, generateEA
    , generateEApar
    -- * Posets
    , module QLogic.Poset
    ) where

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)

import Control.Monad
import Control.Parallel.Strategies

import QLogic.Poset
import QLogic.Poset.Generic
import QLogic.Utils

-- | Class for data types that are quantum logics.
-- Any quantum logic is a partially ordered set,
-- so we have 'POrdStruct' restriction.
--
-- Minimal implementation is 'ocmplIn', but
-- in many cases one can provide more efficient
-- implementation for the remaining functions.
class POrdStruct p a => QLogicStruct p a | p -> a where
    -- | Orthocomplement in quantum logic
    ocmplIn  :: p -> a -> a
    -- | 'True' if two elements are orthogonal in quantum logic.
    orthoIn  :: p -> a -> a -> Bool
    orthoIn ql a b = lessIn ql a (ocmplIn ql b)
    -- | 'True' if two elements are compatible in quantum logic.
    -- Implementation is basically the proof of Proposition 1.3.5 of [1]:
    -- Assume that @a@, @b@ are compatible. Then:
    -- @
    -- a1 = a /\ ortho b, b1 = b /\ ortho a, c = a /\ b
    -- @
    -- (all exists by the assumption), and a1, b1, c are mutually
    --  orthogonal, and satisfy (1a) and (1b). If any of these steps fail
    -- we showed that @a@ and @b@ cannot be compatible (by contradiction).
    --
    -- Contrary, if above procedure resulted in True, we showed explicitly
    -- that @a@ and @b@ are compatible.
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
    -- | Returns the minimal element of quantum logic.
    -- Remark: this function does not check whether it exists!
    zeroOf   :: p -> a
    zeroOf p = head . minimalIn p $ elementsOf p
    -- | Returns the maximal element of quantum logic.
    -- Remark: this function does not check whether it exists!
    oneOf    :: p -> a
    oneOf p = head . maximalIn p $ elementsOf p

-- | General quantum logic data type.
data QLogic p a where
        QLogic :: (Eq a, POrdStruct p a) => p -> (a -> a) -> a -> a -> QLogic p a

instance POrdStruct (QLogic p a) a where
    elementsOf (QLogic poset _ _ _) = elementsOf poset
    lessIn (QLogic poset _ _ _) = lessIn poset
    supIn (QLogic poset _ _ _) = supIn poset

instance POrdStruct p a => QLogicStruct (QLogic p a) a where
    ocmplIn (QLogic _ ocmpl _ _) = ocmpl
    zeroOf (QLogic  _ _ z _) = z
    oneOf (QLogic  _ _ _ o) = o

instance (Show p, Show a) => Show (QLogic p a) where
        show ql@(QLogic poset _ _ _) = show poset ++
            "\nOrthocompletions:\n" ++ (unlines $ olists) ++
            "\nmin: " ++ show (zeroOf ql) ++
            "\nmax: " ++ show (oneOf ql)
            where
                olists = map (\a -> (show a) ++ "C = " ++ (show $ omap a)) $ elementsOf ql
                omap = ocmplIn ql

-- | Constructs 'QLogic' from arbitrary 'POrdStruct'.
-- Note that axioms are not verified.
fromPOrdStruct :: (Eq a, POrdStruct p a) => p -> (a -> a) -> QLogic p a
fromPOrdStruct poset omap = QLogic poset omap min max
    where
        els = elementsOf poset
        min = head $ minimalIn poset els
        max = head $ maximalIn poset els

-- | Convert QLogic to packed representation: elements are replaced
-- by sequence of integers and relation is coverted to array representation.
packQLogic :: (Ord a) => QLogic (Poset a) a -> QLogic (Poset Int) Int
packQLogic (QLogic poset omap min max) = QLogic pposet pomap pmin pmax
    where
        (packed, pposet) = packPoset' poset
        pomap = packFunc packed omap
        pmin = toKey packed min
        pmax = toKey packed max

-- | Convert packed QLogic representation to explicit one.
-- It is not safe: 'Packed' must have appropriate length.
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
quotientQLogic :: (Ord a, QLogicStruct p a) => p -> QLogic (Poset (Equiv a)) (Equiv a)
quotientQLogic ql = QLogic poset equivOcmpl equivMin equivMax
    where
        poset = quotientPoset ql
        els = elementsOf poset
        equivOcmpl = liftFunc els $ ocmplIn ql
        equivMax = fromJust . equivLookup els $ oneOf ql
        equivMin = fromJust . equivLookup els $ zeroOf ql


  
-- | Generate orthomodular sub-lattice of a given quantum logic
generateOML :: (QLogicStruct p a, Ord a) => p -> [a] -> [a]
generateOML ql = Set.toList . fixedSet (generateOML' ql) . Set.fromList

generateOML' :: (QLogicStruct p a, Ord a) => p -> Set a -> Set a
generateOML' ql accum = accum `Set.union` complements `Set.union` sums
  where
    complements = Set.map (ocmplIn ql) accum
    sums = Set.fromList . sums' . Set.toList $ accum
    sums' [] = []
    sums' (a:as) = go as ++ sums' as
      where
        go = map (a \/)
    (\/) = unsafeSupIn ql
    ortho = orthoIn ql

-- | Generate orthomodular sub-poset of a given quantum logic
generateOMP :: (QLogicStruct p a, Ord a) => p -> [a] -> [a]
generateOMP ql = Set.toList . fixedSet (generateOMP' ql) . Set.fromList

generateOMP' :: (QLogicStruct p a, Ord a) => p -> Set a -> Set a
generateOMP' ql accum = accum `Set.union` complements `Set.union` sums
  where
    complements = Set.map (ocmplIn ql) accum
    sums = Set.fromList . sums' . Set.toList $ accum
    sums' [] = []
    sums' (a:as) = go as ++ sums' as
      where
        go = map (a \/) . filter (`ortho` a)
    (\/) = unsafeSupIn ql
    ortho = orthoIn ql

-- | Generate a box sub-effect algebra of a given quantum logic
-- The difference with the standard approach is that the
-- orthogonality relation is different: two elements a b are orthogonal
-- if their orthocomplement decomposes into atoms of the quantum logic 'p'.
-- TODO add more how I do that.
generateEA :: (QLogicStruct p a, Ord a) => p -> [a] -> [a]
generateEA ql qs = Set.toList . fixedSet (generateEA' ql qs) . Set.fromList $ qs

generateEA' :: (QLogicStruct p a, Ord a) => p -> [a] -> Set a -> Set a
generateEA' ql !atoms !accum = accum `Set.union` complements `Set.union` sums
  where
    complements = Set.map (ocmplIn ql) accum
    sums = Set.fromList . sums' . Set.toList $ accum
    sums' [] = []
    sums' !(a:as) = go as ++ sums' as
      where
        go !ls = filter (not . null . decomposeInto ql atoms . ocmplIn ql)
                 . map (a \/) . filter (`ortho` a) $ ls
    (\/) = unsafeSupIn ql
    ortho = orthoIn ql

generateEApar :: (QLogicStruct p a, Ord a, NFData a) => p -> [a] -> [a]
-- generateEApar ql qs = Set.toList . fixedSet (generateEApar' ql qs) . Set.fromList $ qs
generateEApar ql qs = fixedList (generateEApar'' ql qs) qs

generateEApar' :: (QLogicStruct p a, Ord a, NFData a) => p -> [a] -> Set a -> Set a
generateEApar' ql !atoms !accum = accum `Set.union` complements `Set.union` sums
  where
    complements = Set.map (ocmplIn ql) accum
    sums = Set.fromList . sums' . Set.toList $ accum
    sums' [] = []
    sums' !(a:as) = (go as `using` parListChunk 100 rdeepseq) ++ sums' as
      where
        go !ls = filter (not . null . decomposeInto ql atoms . ocmplIn ql)
                 . map (a \/) . filter (`ortho` a) $ ls
    (\/) = unsafeSupIn ql
    ortho = orthoIn ql

generateEApar'' :: (QLogicStruct p a, Ord a, NFData a) => p -> [a] -> [a] -> [a]
generateEApar'' ql !atoms !accum = rmdups $ accum ++ complements ++ sums accum
  where
    complements = map (ocmplIn ql) accum
    sums [] = []
    sums !(a:as) = go as ++ sums as
    -- sums !(a:as) = (go as `using` r0) ++ sums as
      where
        go !ls = filter (not . null . decomposeInto ql atoms . ocmplIn ql)
                 . map (a \/) . filter (`ortho` a) $ ls
    (\/) = unsafeSupIn ql
    ortho = orthoIn ql

fixedSet :: (Set a -> Set a) -> Set a -> Set a
fixedSet f x | Set.size next == Set.size x = x
             | otherwise = fixedSet f next
  where
    next = f x

fixedList :: ([a] -> [a]) -> [a] -> [a]
fixedList f x | length next == length x = x
              | otherwise = fixedList f next
  where
    next = f x

rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c

-- | List of atoms in logic,
-- i.e. elements covering zero.
atomsOf :: QLogicStruct p a => p -> [a]
atomsOf ql = minimalIn ql $ filter (not . (`equal` zero)) $ elementsOf ql
    where
        equal = equalIn ql
        zero = zeroOf ql

-- |Checks if given structure is a quantum logic.
isLogic :: QLogicStruct p a => p -> Bool
isLogic set = and [isOrderReverse set,
                   isOrthoIdempotence set,
                   isSupremum set,
                   isOrthomodular set]

-- | Checks L2 axiom of a quantum logic.
isOrderReverse :: QLogicStruct p a => p -> Bool
isOrderReverse ql = and cond
    where
        cond = map id [ocmpl q ≤ ocmpl p | p <- elementsOf ql,
                                           q <- elementsOf ql,
                                           p ≤ q] `using` parList rdeepseq
        (≤) = lessIn ql
        ocmpl = ocmplIn ql

-- | Checkis L3 axiom of a quantum logic.
isOrthoIdempotence :: QLogicStruct p a => p -> Bool
isOrthoIdempotence ql = and cond
    where
        cond = map idem (elementsOf ql) `using` parList rdeepseq
        idem p = equalIn ql p (ocmpl . ocmpl $ p)
        ocmpl = ocmplIn ql

-- | Checks L4 axiom of a quantum logic.
isSupremum :: QLogicStruct p a => p -> Bool
isSupremum ql = all isJust [supIn ql p q | p <- elementsOf ql,
                                              q <- elementsOf ql,
                                              orthoIn ql p q]

-- | Checks L5 axiom of a quantum logic.
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

-- | Checks L5 axiom of quantum logic and return
-- first element for which it fails.
isOrthoModular' :: QLogicStruct p a => p -> Either (a, a) Bool
isOrthoModular' ql = case notOMLPairs of
                        [] -> Right True
                        p:_ -> Left p
  where
    notOMLPairs = [(a, b) | a <- els, b <- els, not (orthomodularLaw ql a b)]
    els = elementsOf ql

orthomodularLaw :: QLogicStruct p a => p -> a -> a -> Bool
orthomodularLaw ql a b
    | not $ a .<. b = True
    | otherwise     = b === ((b /\ ocmpl a) \/ a)
        where
            (.<.) = lessIn ql
            (\/) = unsafeSupIn ql
            (/\) = unsafeInfIn ql
            (===) = equalIn ql
            ocmpl = ocmplIn ql

-- | Checks if all sups and infs exist.
hasAllSups :: QLogicStruct p a => p -> Bool
hasAllSups ql = sups && infs
    where
        sups = all isJust [supIn ql a b | a <- els, b <- els] -- `using` parList rdeepseq
        infs = all isJust [infIn ql a b | a <- els, b <- els] -- `using` parList rdeepseq
        els = elementsOf ql

-- |Checks if the logic is orthomodular lattice.
isLattice :: QLogicStruct p a => p -> Bool
isLattice ql = isLogic ql && hasAllSups ql

-- | Checks distributivity law
-- Remark: it doesn't check if sups and infs exists but assumes
-- existence. Do 'hasAllSups' before for safe behaviour.
isDistributive :: QLogicStruct p a => p -> Bool
isDistributive ql = distrib
    where
        distrib = and [((a \/ b) /\ c) === ((a /\ c) \/ (b /\ c)) | a <- els, b <- els, c <- els]
        els = elementsOf ql
        (\/) = unsafeSupIn ql
        (/\) = unsafeInfIn ql
        (===) = equalIn ql

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

-- | Checks if a given logic is a Boolean logic.
isBoolean :: QLogicStruct p a => p -> Bool
isBoolean ql = mutuallyCompatibleIn ql $ elementsOf ql

-- | Checks if elemensts in subset are mutually disjoint.
mutuallyDisjointIn :: QLogicStruct p a => p -> [a] -> Bool
mutuallyDisjointIn ql = mutuallyBy (orthoIn ql)

-- | Checks if elements in subset are mutually orthogonal.
mutuallyCompatibleIn :: QLogicStruct p a => p -> [a] -> Bool
mutuallyCompatibleIn ql = mutuallyBy (compatIn ql)

-- | Returns all decomopsitions into atoms for given element of the logics.
-- Clearly, it has to be atomistic.
atomicDecomposition :: (QLogicStruct p a) => p -> a -> [[a]]
atomicDecomposition ql a = filter (sumUpTo a) . subsetsBy (orthoIn ql) . atomsOf $ ql
    where
        sumUpTo a [] = False
        sumUpTo a (b:bs) = foldl' (\/) b bs === a
        (\/) = unsafeSupIn ql
        (===) = equalIn ql

-- TODO: move to QLogic and replace/complement atomicDecompositions
decompose :: (QLogicStruct p a) => p -> a -> [a]
decompose ql q = decompose' ql [] q (atomsOf ql)

decompose' :: (QLogicStruct p a) => p -> [a] -> a -> [a] -> [a]
decompose' _ !accum _ [] = accum
decompose' ql !accum q (a:as)
  | a .<=. q  = decompose' ql (a:accum) q $ filter (`ortho` a) as
  | otherwise = decompose' ql accum q as
    where
      (.<=.) = lessIn ql
      ortho = orthoIn ql

decomposeInto :: (QLogicStruct p a) => p -> [a] -> a -> Maybe [a]
decomposeInto ql atoms q = decomposeInto' ql [] q atoms

decomposeInto' :: (QLogicStruct p a) => p -> [a] -> a -> [a] -> Maybe [a]
decomposeInto' ql !accum q []
  | equalIn ql (sup accum) q = Just accum
  | otherwise = Nothing
  where
    sup [] = zeroOf ql
    sup as = foldl1' (\/) accum
    (\/) = unsafeSupIn ql
decomposeInto' ql !accum q (a:as)
  | a .<=. q  = decomposeInto' ql (a:accum) q $ filter (`ortho` a) as
  | otherwise = decomposeInto' ql accum q as
    where
      (.<=.) = lessIn ql
      ortho = orthoIn ql
