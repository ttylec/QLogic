{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QStructures.Hilbert where

import Prelude hiding (replicate)
import Data.Serialize
import Data.Maybe
import qualified Data.Text as T
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
import GHC.Generics
import QStructures
import Data.Ratio
import QStructures.IO

data Complex a = C !a !a deriving (Eq, Ord, Generic)

instance Serialize a => Serialize (Complex a)


instance Show a => Show (Complex a) where
  show (C r i) = show r ++ "+" ++ show i ++ "i"

instance MathematicaForm a => MathematicaForm (Complex a) where
  mform (C r i) = mform r `T.append` "+" `T.append`
    mform i `T.append` "I"

instance Num a => Num (Complex a) where
  (C x1 y1) + (C x2 y2) = C (x1 + x2) (y1 + y2)
  (C x1 y1) * (C x2 y2) = C (x1*x2 - y1*y2) (x1*y2 + x2*y1)
  negate (C x1 y1) = C (negate x1) (negate y1)
  fromInteger n = C (fromInteger n) 0
  abs (C x y) = C (x*x + y*y) 0
  signum (C x y) = 0

real :: Num a => a -> Complex a
real x = C x 0

imag :: Num a => a -> Complex a
imag x = C 0 x

newtype Matrix (n :: Nat) s = Matrix (V n (V n s)) deriving (Eq, Ord, Show, Generic)

instance (Dim n, Serialize s) => Serialize (Matrix n s)

instance (Dim n, MathematicaForm a) => MathematicaForm (Matrix n a) where
  mform (Matrix a) = formatRows . V.map formatRow . toVector $ a
    where
      formatRow :: (Dim n, MathematicaForm a) => V n a -> T.Text
      formatRow r = "{" `T.append`
        (T.intercalate ", " . V.toList . toVector $ fmap mform r) `T.append`
        "}"
      formatRows m = "{\n" `T.append`
        (T.intercalate "\n," . V.toList $ m) `T.append`
        "}"

instance Dim n => Dim (Matrix n a) where
  reflectDim _ = reflectDim (Proxy :: Proxy n)
  {-# INLINE reflectDim #-}

matrix :: forall n a. Dim n => [[a]] -> Maybe (Matrix n a)
matrix ls
  | length ls == dim && all (==dim) rowlengths = Matrix <$> (rows >>= fromVector)
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
tensor (Matrix !a) (Matrix !b) = Matrix . flatten . fmap (fmap (*!! b)) $ a

flatten :: (KnownNat (n*m), KnownNat n, KnownNat m) =>
           V n (V n (V m (V m a))) -> V (n*m) (V (n*m) a)
flatten = rowconcat . fmap (fmap rowconcat . transpose)

rowconcat :: (KnownNat n, KnownNat m, KnownNat (n*m)) => V n (V m a) -> V (n*m) a
rowconcat = fromJust . fromVector . G.unstream . Bundle.concatVectors .
  Bundle.map toVector . Bundle.fromVector . toVector

data HilbertOMP n s = HilbertOMP !Int ![Matrix n s] deriving (Generic)
data HilbertBoxEA n s = HilbertBoxEA !Int ![Matrix n s] ![Matrix n s]
  deriving (Generic)

instance (Dim n, Serialize s) => Serialize (HilbertOMP n s)
instance (Dim n, Serialize s) => Serialize (HilbertBoxEA n s)

projOrdering :: (Dim n, Eq s, Num s) => Matrix n s -> Matrix n s -> Bool
projOrdering (Matrix a) (Matrix b) = a !*! b == a && b !*! a == a

projOrthogonal :: (Dim n, Eq s, Num s) => Matrix n s -> Matrix n s -> Bool
projOrthogonal (Matrix a) (Matrix b) = Matrix (a !*! b) == zero

instance (Eq s, Num s, Dim n) => QStruct (HilbertOMP n s) (Matrix n s) where
  elementsOf (HilbertOMP _ els) = els
  orthoIn _ = projOrthogonal
  oplusIn omp a b
    | orthoIn omp a b = Just $ a ^+^ b
    | otherwise = Nothing
  zeroOf (HilbertOMP n _) = zero  -- Matrix $ V zero
  oneOf (HilbertOMP n _) = Matrix $ identity
  equalIn _ = (==)
  lessIn _ = projOrdering
  ocmplIn omp a = oneOf omp ^-^ a

instance (Eq s, Num s, Dim n) => QStruct (HilbertBoxEA n s) (Matrix n s) where
  elementsOf (HilbertBoxEA _ _ els) = els
  orthoIn ea@(HilbertBoxEA _ atoms _) ma@(Matrix a) mb@(Matrix b) =
    projOrthogonal ma mb && (cmpl == zero || decomposable cmpl)
    where
      cmpl = ocmplIn ea . Matrix $ a ^+^ b
      decomposable = not . null . decomposeBy projOrdering projOrthogonal atoms
  oplusIn omp a b
    | orthoIn omp a b = Just $ a ^+^ b
    | otherwise = Nothing
  zeroOf ea = zero
  oneOf ea = Matrix $ identity
  equalIn _ = (==)
  lessIn _ = projOrdering
  ocmplIn omp (Matrix !a) = Matrix $ identity ^-^ a

instance (Dim n, Num a, Ord a) => OMP (HilbertOMP n a) (Matrix n a)
instance (Dim n, Num a, Ord a) => EA (HilbertBoxEA n a) (Matrix n a)
