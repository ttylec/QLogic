{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module QStructures.Hilbert where

import Prelude hiding (replicate)
import Data.Maybe
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Complex
import Data.Proxy
import qualified Data.Vector as V
import Linear.V
import Linear.Matrix
import Linear.Vector

import QStructures

data Matrix n s = Matrix (V n (V n s)) deriving (Eq, Ord, Show)


instance Functor (Matrix n) where
  fmap f (Matrix m) = Matrix $ (fmap (fmap f) m)
  {-# INLINE fmap #-}

instance (Dim n) => Applicative (Matrix n) where
  pure = Matrix . pure . pure
  {-# INLINE pure #-}

  (Matrix (V as)) <*> (Matrix (V bs)) = Matrix $ V (V.zipWith (<*>) as bs)
  {-# INLINE (<*>) #-}

instance Dim n => Additive (Matrix n) where
  zero = Matrix $ V zero
  (Matrix a) ^+^ (Matrix b) = Matrix $ a ^+^ b


data HilbertOMP n s = HilbertOMP Int [Matrix n s]

instance (Num s, Ord s) => Ord (Complex s) where
  compare a b = compare (realPart a) (realPart b) `mappend`
                compare (imagPart a) (imagPart b)

-- instance (Num s, Ord s, Dim n) => Ord (Matrix n s) where
--   compare a b = compare (flatten a) (flatten b)
--     where
--       flatten :: Matrix n s -> Vector s
--       flatten = fmap toVector . toVector

-- zero :: (Dim n, Num s) => n -> Matrix n s
-- zero n = (n><n) $ repeat 0

instance (Eq s, Num s, Dim n) => QStruct (HilbertOMP n s) (Matrix n s) where
  elementsOf (HilbertOMP _ els) = els
  orthoIn omp (Matrix a) (Matrix b) = Matrix (a !*! b) == zero
  oplusIn omp a b
    | orthoIn omp a b = Just $ a ^+^ b
    | otherwise = Nothing
  zeroOf (HilbertOMP n _) = zero  -- Matrix $ V zero
  oneOf (HilbertOMP n _) = Matrix $ identity
  equalIn _ = (==)
  lessIn _ (Matrix a) (Matrix b) = a !*! b == a && b !*! a == a
  ocmplIn omp a = oneOf omp ^-^ a

-- TODO there is some strange error when trying to define it polymorphic
-- instance (Num s, Element s, Numeric s) => QStruct (HilbertOMP s) (Matrix s) where
--   elementsOf (HilbertOMP _ els) = els
--   orthoIn (HilbertOMP n _) a b = a <> b == zero n
--   oplusIn omp a b
--     | orthoIn omp a b = Just $ a + b
--     | otherwise = Nothing
--   zeroOf (HilbertOMP n _) = zero n
