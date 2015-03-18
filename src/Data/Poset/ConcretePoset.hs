{-# LANGUAGE GADTs, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Data.Poset.ConcretePoset (ConcretePoset(..), ConcretePosetInt(..)) where

import Data.Set (Set)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import Data.Poset

-- | A concrete poset 
data ConcretePoset a where
   ConcretePoset :: (Ord a) => [Set a] -> ConcretePoset a

instance (Ord a) => POrdStruct (ConcretePoset a) (Set a) where
    elementsOf (ConcretePoset els) = els
    lessIn _  = Set.isSubsetOf
    supIn poset a b 
        | Set.intersection a b == Set.empty = Just $ Set.union a b
        | length lub == 1 = Just $ head lub
        | otherwise = Nothing
          where
              lub = lubIn poset a b

data ConcretePosetInt = ConcretePosetInt [IntSet]

instance POrdStruct ConcretePosetInt IntSet where
    elementsOf (ConcretePosetInt els) = els
    lessIn _  = IntSet.isSubsetOf
    supIn poset a b 
        | IntSet.intersection a b == IntSet.empty = Just $ IntSet.union a b
        | length lub == 1 = Just $ head lub
        | otherwise = Nothing
          where
              lub = lubIn poset a b

