{-# LANGUAGE GADTs, BangPatterns #-}

module Data.Relation (Relation(Function, ListRel, ArrayRel)
                     , inRelation
                     , packRelation
                     , isAntiSymmetric, isSymmetric
                     , isTransitive, isReflexive)
                     where


import Control.Monad.Identity
import Data.Array.Repa hiding ((++), map)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Poset.Internals
import Data.QLogic.Utils

-- |Represent partial order relation.
-- Relation can be given either as: a function,
-- a list of elements that are in relation 
-- for any given element, or by matrix with boolean entries.
data Relation a where
        Function :: (Eq a) => (a -> a -> Bool) -> Relation a
        ListRel  :: (Eq a, Ord a) => Map a [a] -> Relation a
        ArrayRel :: Array U DIM2 Bool -> Relation Int 

-- |Convert different types of relation to array representation
packRelation :: (Ord a) => Packed a -> Relation a -> Relation Int
packRelation packed rel = ArrayRel $ runIdentity $ computeUnboxedP (fromFunction (Z:.n:.n) isLess)
    where
        isLess (Z:.i:.j) = packFunc2 packed (inRelation rel) i j
        n = length $ packedElements packed 

inRelation :: Relation a -> a -> a -> Bool
inRelation (Function f) = f
inRelation (ListRel rel) = \a b -> case Map.lookup a rel of
                                         Nothing -> False
                                         Just x -> b `elem` x 
inRelation (ArrayRel rel) = \i j -> rel ! (Z:.i:.j)

isAntiSymmetric :: (Eq a) => [a] -> Relation a -> Bool
isAntiSymmetric els rel = and [(x `r` y && y `r` x) `implies` (x == y) | x <- els, y <- els]
    where
        r = inRelation rel

isSymmetric :: (Eq a) => [a] -> Relation a -> Bool
isSymmetric els rel = and [(x `r` y) `implies` (y `r` x) | x <- els, y <- els]
    where
        r = inRelation rel

isTransitive :: (Eq a) => [a] -> Relation a -> Bool
isTransitive els rel = and [(x `r` y && y `r` z) `implies` (x `r` z) | 
                           x <- els, y <- els, z <- els]
    where
        r = inRelation rel

isReflexive :: (Eq a) => [a] -> Relation a -> Bool
isReflexive els rel = and [x `r` x | x <- els]
    where
        r = inRelation rel

