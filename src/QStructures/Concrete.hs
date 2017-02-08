{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module QStructures.Concrete where

import Data.Maybe
import Data.Set (Set)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Map (Map)

import QStructures

-- | Concrete structure over an abstract set
data ConcreteOMP a = ConcreteOMP (Set a) [Set a]

data ConcreteBoxEA a = ConcreteBoxEA (Set a) [Set a] [Set a]

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = Set.null $ Set.intersection a b

instance Ord a => QStruct (ConcreteBoxEA a) (Set a) where
  elementsOf (ConcreteBoxEA _ _ els) = els
  orthoIn ea@(ConcreteBoxEA _ atoms _ ) a b =
    disjoint a b && (cmpl == Set.empty || decomposable cmpl)
    where
      cmpl = ocmplIn ea $ Set.union a b
      decomposable = not . null . decomposeBy Set.isSubsetOf disjoint atoms
  oplusIn ea@(ConcreteBoxEA _ atoms _) a b
    | orthoIn ea a b = Just $ Set.union a b
    | otherwise = Nothing
  zeroOf _ = Set.empty
  oneOf (ConcreteBoxEA space _ _) = space
  equalIn _  = (==)
  lessIn _  = Set.isSubsetOf
  supIn omp a b
    | orthoIn omp a b = oplusIn omp a b
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
      lub = lubIn omp [a, b]
  ocmplIn ea = Set.difference (oneOf ea)

instance Ord a => EA (ConcreteBoxEA a) (Set a)

instance Ord a => QStruct (ConcreteOMP a) (Set a) where
  elementsOf (ConcreteOMP _ els) = els
  orthoIn _ a b = Set.null $ Set.intersection a b
  oplusIn omp a b
    | orthoIn omp a b =  Just $ Set.union a b
    | otherwise = Nothing
  zeroOf _ = Set.empty
  oneOf (ConcreteOMP space _) = space
  equalIn _  = (==)
  lessIn _  = Set.isSubsetOf
  supIn omp a b
    | orthoIn omp a b = oplusIn omp a b
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
      lub = lubIn omp [a, b]
  ocmplIn ql = Set.difference (oneOf ql)

instance Ord a => OMP (ConcreteOMP a) (Set a)

instance Ord a => EA (ConcreteOMP a) (Set a)
