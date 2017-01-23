{- |
Module      :  $Header$
Description :  QLogic class and data type and basic operations
Copyright   :  (c) Tomasz Tylec
License     :  MIT

Maintainer  :  ttylec@gmail.com
Stability   :  experimental
Portability :  non-portable (MPTC, FD, FI)

-- TODO update docs

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

module QStructures ( QStruct, EA, OMP, OML
                   , elementsOf, oplusIn, zeroOf, oneOf
                   , equalIn, lessIn, orthoIn, ocmplIn
                   , supIn, infIn
                   , lubIn, glbIn
                   , decompose
                   , generateSubStructure) where

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)

import Control.Monad

class QStruct p a | p -> a where
  elementsOf :: p -> [a]
  oplusIn  :: p -> a -> a -> Maybe a
  zeroOf   :: p -> a
  oneOf    :: p -> a

  equalIn  :: p -> a -> a -> Bool
  lessIn   :: p -> a -> a -> Bool
  orthoIn  :: p -> a -> a -> Bool
  ocmplIn  :: p -> a -> a
  supIn    :: p -> a -> a -> Maybe a
  infIn    :: p -> a -> a -> Maybe a
  supIn qs a b
    | orthoIn qs a b = oplusIn qs a b
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
      lub = lubIn qs [a, b]
  infIn qs a b
    | length glb == 1 = Just $ head glb
    | otherwise = Nothing
    where
      glb = glbIn qs [a, b]

class QStruct p a => EA p a

class QStruct p a => OMP p a

class QStruct p a => OML p a


-- | Least upper bounds of list of elements,
-- i.e. minimal elements in the set of elements
-- that are greater than all.
lubIn :: QStruct p a => p -> [a] -> [a]
lubIn poset as = minimalIn poset $ lubIn' poset as (elementsOf poset)

lubIn' _ [] pool = pool
lubIn' poset (a:as) pool = lubIn' poset as $ filter (a ≤) pool
    where
        (≤) = lessIn poset

-- | Returns minimal elements in a given list,
-- i.e. elements that are not greater than any of the
-- other elements of the set.
minimalIn :: QStruct p a => p -> [a] -> [a]
minimalIn _ [] = []
minimalIn _ [a] = [a]
minimalIn poset (a:as)
    | any (≤ a) as = minimalIn poset as
    | otherwise = a:(minimalIn poset $ filter (not . (a ≤)) as)
    where
        (≤) = lessIn poset

-- | Set of greatest lower bounds of two elements
-- i.e. maximal elements in the set of elements
-- that are less than both.
glbIn :: QStruct p a => p -> [a] -> [a]
glbIn poset as = maximalIn poset $ glbIn' poset as (elementsOf poset)

glbIn' _ [] pool = pool
glbIn' poset (a:as) pool = glbIn' poset as (filter (≤ a) pool)
    where
        (≤) = lessIn poset

-- |Returns subset of maximal elements in given set,
-- i.e. element that are not less than any of the
-- other lements of the set.
maximalIn :: QStruct p a => p -> [a] -> [a]
maximalIn _ [] = []
maximalIn _ [a] = [a]
maximalIn poset (a:as)
    | any (a ≤) as = maximalIn poset as
    | otherwise = a:(maximalIn poset $ filter (not . (≤ a)) as)
    where
        (≤) = lessIn poset

decompose :: (QStruct p a) => p -> [a] -> a -> [a]
decompose ql atoms q = decompose' ql [] q atoms
  where
    decompose' _ !accum _ [] = accum
    decompose' ql !accum q (a:as)
      | a .<=. q  = decompose' ql (a:accum) q $ filter (`ortho` a) as
      | otherwise = decompose' ql accum q as
    (.<=.) = lessIn ql
    ortho = orthoIn ql

-- TODO: we need SCC here, since that implementation
-- might be wrong (I see no reason why incremental build
-- should be sufficient).
-- decomposeInto :: (QStruct p a) => p -> [a] -> a -> Maybe [a]
-- decomposeInto ql atoms q = decomposeInto' ql [] q atoms

-- decomposeInto' :: (QStruct p a) => p -> [a] -> a -> [a] -> Maybe [a]
-- decomposeInto' ql !accum q []
--   | equalIn ql (sup accum) q = Just accum
--   | otherwise = Nothing
--   where
--     sup [] = zeroOf ql
--     sup (a:as) = fromJust $ foldM (\/) a as
--     (\/) = supIn ql
--     (==) = equalIn ql
-- decomposeInto' ql !accum q (a:as)
--   | a .<=. q  = decomposeInto' ql (a:accum) q $ filter (`ortho` a) as
--   | otherwise = decomposeInto' ql accum q as
--     where
--       (.<=.) = lessIn ql
--       ortho = orthoIn ql

generateSubStructure :: (QStruct p a, Ord a) => p -> [a]
generateSubStructure struct = Set.toList . fixedSet (subGenerator struct) . Set.fromList . elementsOf $ struct

subGenerator :: (QStruct p a, Ord a) => p -> Set a -> Set a
subGenerator struct accum = accum `Set.union` complements `Set.union` sums
  where
    complements = Set.map (ocmplIn struct) accum
    sums = Set.fromList . sums' . Set.toList $ accum
    sums' [] = []
    sums' (a:as) = go as ++ sums' as
      where
        go = map (fromJust . oplusIn struct a) . filter (orthoIn struct a)


-- | Iterate function as long as result changes, specialized for sets.
fixedSet :: (Set a -> Set a) -> Set a -> Set a
fixedSet f x | Set.size next == Set.size x = x
             | otherwise = fixedSet f next
  where
    next = f x
