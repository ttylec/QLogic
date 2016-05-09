{-# LANGUAGE GADTs, BangPatterns #-}

 module QLogic.Relation (Relation(Function, ListRel, ArrayRel)
                         , inRelation
                         , packRelation, sparseRelation
                         , relationFromFuncM
                         , isAntiSymmetric, isSymmetric
                         , isTransitive, isReflexive
                         , transitiveClosure)
        where

import Debug.Trace

import Data.List
import Control.Monad (filterM)
import Control.Monad.Identity (runIdentity)
import Data.Array.Repa hiding ((++), map, traverse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Parallel version of transitiveClosure for
-- ListRel needs following imports:
--
-- import Control.Monad.Par
-- import Data.Traversable hiding (mapM)
--
-- But we need to make everything an instance of NFData
-- as well. Maybe it's not worth, because "packed" works
-- well and is much faster than transitive closure on ListRel.

import QLogic.Utils

-- | Represent partial order relation.
-- Relation can be given either as: 
--  * a function,
--  * a list of elements that are in relation 
--    for any given element,
--  * a matrix with boolean entries.
--
--  The last representation uses Repa Array
--  to store the array. It allows to use Repa's
--  parallel capabilities. See 'transitiveClosure'.
data Relation a where
        Function :: (a -> a -> Bool) -> Relation a
        ListRel  :: Ord a => Map a [a] -> Relation a
        ArrayRel :: Array U DIM2 Bool -> Relation Int 

-- | Convert relation to array representation.
packRelation :: (Ord a) => Packed a -> Relation a -> Relation Int
packRelation packed rel = ArrayRel $ runIdentity $ computeUnboxedP (fromFunction (Z:.n:.n) isLess)
    where
        isLess (Z:.i:.j) = packFunc2 packed (inRelation rel) i j
        n = length $ packedElements packed 

-- |Convert relation to list representation.
sparseRelation :: (Ord a) => [a] -> Relation a -> Relation a
sparseRelation els rel = ListRel $ Map.fromList inrels
    where
        inrels = zip els $ map inRelList els
        inRelList e = filter (inRelation rel e) els

-- |Construct sparse relation from monadic function
relationFromFuncM :: (Ord a, Monad m) => [a] -> (a -> a -> m Bool) -> m (Relation a)
relationFromFuncM els f = do
        inrel <- sequence $ map geListM els
        return $ ListRel $ Map.fromList $ zip els inrel
        where
            geListM a = filterM (a `f`) els

-- |Returns whether two elements are in relation.
inRelation :: Relation a -> a -> a -> Bool
inRelation (Function f) = f
inRelation (ListRel rel) = inListRelation rel
inRelation (ArrayRel rel) = \i j -> rel ! (Z:.i:.j)

inListRelation :: (Ord k, Eq a) => Map k [a] -> k -> a -> Bool
inListRelation rel = \a b -> case Map.lookup a rel of
                                 Nothing -> False
                                 Just x -> b `elem` x 

-- * Properties of relations.

-- | Tests if the relation is anti-symmetric,
-- i.e. @x `rel` y@ and @y `rel` x@ implies @x == y@.
isAntiSymmetric :: (Eq a) => [a] -> Relation a -> Bool
isAntiSymmetric els rel = and [(x `r` y && y `r` x) `implies` (x == y) | x <- els, y <- els]
    where
        r = inRelation rel

-- | Tests if the relation is symmetric.
-- i.e. @x `rel` y@ implies @y `rel` x@.
isSymmetric :: (Eq a) => [a] -> Relation a -> Bool
isSymmetric els rel = and [(x `r` y) `implies` (y `r` x) | x <- els, y <- els]
    where
        r = inRelation rel

-- | Tests if the relation is transitive,
-- i.e. @x `rel` y@ and @y `rel` z@ implies @x `rel` z@.
isTransitive :: (Eq a) => [a] -> Relation a -> Bool
isTransitive els rel = and [(x `r` y && y `r` z) `implies` (x `r` z) | 
                           x <- els, y <- els, z <- els]
    where
        r = inRelation rel

-- | Tests if the relation is reflexive,
-- i.e. @x `rel` x@.
isReflexive :: (Eq a) => [a] -> Relation a -> Bool
isReflexive els rel = and [x `r` x | x <- els]
    where
        r = inRelation rel

-- | Compute transitive closure of the relation.
-- Code based on the implementaion of Floyd-Warshall algorithm
-- for lengths of shortests paths found in
-- S. Marlow, Parallel and Concurrent Programming in Haskell
--
-- Implemented only for 'ArrayRel' and 'ListRel' constructors.
-- 'Function' is left undefined on purpose: transitive closure 
-- of 'ArrayRel' is much faster that 'ListRel' but also
-- need much more memory. So whether the 'Function'
-- should be converted to 'ArrayRel' or 'ListRel'
-- is up to the human.
transitiveClosure :: (Eq a, Ord a) => [a] -> Relation a -> Relation a
transitiveClosure _ (ArrayRel rel) = ArrayRel $ runIdentity $ go rel 0
    where
        Z :. _ :. n = extent rel
        go !g !k 
            | k == n    = return g
            | otherwise = do
                g' <- computeP (fromFunction (Z:.n:.n) sp)
                go g' (k+1)
                where
                    sp (Z:.i:.j) = (g ! (Z:.i:.j)) || ((g ! (Z:.i:.k)) && (g ! (Z:.k:.j)))
transitiveClosure els (ListRel rel) = ListRel $ foldl' update rel els
    where
        update rel k = Map.mapWithKey transit rel
        -- The commented code introduce parallelism, but requires everything to be
        -- an instance of NFData...
        -- runPar $ do
        --     m <- Map.traverseWithKey (\a gelist -> spawn (return (transit a gelist))) rel
        --     traverse get m 
            where 
                  transit a gelist = foldr greater [] els
                      where greater j m | j `elem` gelist = j:m
                                        | inListRelation rel a k && inListRelation rel k j = j:m
                                        | otherwise = m
transitiveClosure els (Function rel) = undefined
