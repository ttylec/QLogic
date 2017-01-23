{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module QStructures.Hilbert where

import Data.Maybe
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Map (Map)
import Numeric.LinearAlgebra

import QStructures

data HilbertOMP s = HilbertOMP Int [Matrix s]

instance (Num s, Ord s) => Ord (Complex s) where
  compare a b = compare (realPart a) (realPart b) `mappend`
                compare (imagPart a) (imagPart b)

instance (Num s, Ord s, Container Vector s) => Ord (Matrix s) where
  compare a b = compare (toList . flatten $ a) (toList . flatten $ b)

zero n = (n><n) $ repeat 0

instance QStruct (HilbertOMP C) (Matrix C) where
  elementsOf (HilbertOMP _ els) = els
  orthoIn (HilbertOMP n _) a b = a <> b == zero n
  oplusIn omp a b
    | orthoIn omp a b = Just $ a + b
    | otherwise = Nothing
  zeroOf (HilbertOMP n _) = zero n
  oneOf (HilbertOMP n _) = ident n
  equalIn _ = (==)
  lessIn _ a b = a <> b == a && b <> a == a
  ocmplIn (HilbertOMP n _) a = ident n - a

-- TODO there is some strange error when trying to define it polymorphic
-- instance (Num s, Element s, Numeric s) => QStruct (HilbertOMP s) (Matrix s) where
--   elementsOf (HilbertOMP _ els) = els
--   orthoIn (HilbertOMP n _) a b = a <> b == zero n
--   oplusIn omp a b
--     | orthoIn omp a b = Just $ a + b
--     | otherwise = Nothing
--   zeroOf (HilbertOMP n _) = zero n
