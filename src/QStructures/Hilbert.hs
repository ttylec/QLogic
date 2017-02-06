{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module QStructures.Hilbert where

import Prelude hiding (replicate)
import Data.Maybe
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Generic as G
import Linear.V
import Linear.Matrix
import Linear.Vector
import GHC.TypeLits
import QStructures


data Complex a = C !a !a deriving (Eq, Ord)

instance Show a => Show (Complex a) where
  show (C r i) = show r ++ " + " ++ show i ++ " i"

real :: Num a => a -> Complex a
real x = C x 0

imag :: Num a => a -> Complex a
imag x = C 0 x

instance Num a => Num (Complex a) where
  (C x1 y1) + (C x2 y2) = C (x1 + x2) (y1 + y2)
  (C x1 y1) * (C x2 y2) = C (x1*x2 - y1*y2) (x1*y2 + x2*y1)
  negate (C x1 y1) = C (negate x1) (negate y1)
  fromInteger n = C (fromInteger n) 0
  abs (C x y) = C (x*x + y*y) 0
  signum (C x y) = 0

newtype Matrix (n :: Nat) s = Matrix (V n (V n s)) deriving (Eq, Ord, Show)

instance Dim n => Dim (Matrix n a) where
  reflectDim _ = reflectDim (Proxy :: Proxy n)
  {-# INLINE reflectDim #-}

matrix :: forall n a. Dim n => [[a]] -> Maybe (Matrix n a)
matrix ls
  | length ls == dim && all (==dim) rowlengths = Matrix <$> (rows >>= fromVector)
  -- | length ls == dim && all (==dim) rowlengths = rows >>= fromVector
  | otherwise = Nothing
  where
    dim = reflectDim (Proxy :: Proxy n)
    rowlengths = map length ls
    rows = V.fromList <$> (sequence $ map (fromVector . V.fromList) ls)

instance Functor (Matrix n) where
  fmap f (Matrix m) = Matrix $ (fmap (fmap f) m)
  {-# INLINE fmap #-}

instance (Dim n) => Applicative (Matrix n) where
  pure = Matrix . pure . pure
  {-# INLINE pure #-}

  (Matrix (V as)) <*> (Matrix (V bs)) = Matrix $ V (V.zipWith (<*>) as bs)
  {-# INLINE (<*>) #-}

instance Dim n => Additive (Matrix n) where
  zero = pure 0
  (Matrix a) ^+^ (Matrix b) = Matrix $ a ^+^ b
  {-# INLINE (^+^) #-}

tensor :: (KnownNat (n*m), KnownNat n, KnownNat m, Num a)
       => Matrix n a -> Matrix m a -> Matrix (n*m) a
tensor (Matrix a) (Matrix b) = Matrix . flatten . fmap (fmap (*!! b)) $ a


flatten :: (KnownNat (n*m), KnownNat n, KnownNat m) =>
           V n (V n (V m (V m a))) -> V (n*m) (V (n*m) a)
flatten = rowconcat . fmap (fmap rowconcat . transpose)

rowconcat :: (KnownNat n, KnownNat m, KnownNat (n*m)) => V n (V m a) -> V (n*m) a
rowconcat = fromJust . fromVector . G.unstream . Bundle.concatVectors .
  Bundle.map toVector . Bundle.fromVector . toVector

data HilbertOMP n s = HilbertOMP Int [Matrix n s]

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
