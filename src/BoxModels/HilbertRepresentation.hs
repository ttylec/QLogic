{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module BoxModels.HilbertRepresentation where

import GHC.TypeLits
import Data.Maybe
import Data.List
import Data.Ratio
import Data.Foldable hiding (toList)

import QStructures.Hilbert
import BoxModels
import Linear.V
import Linear.Vector
import qualified Data.Vector as V

-- Binary box representation

pX 0 = fromJust . matrix $
  [
    [C 1 0, C 0 0]
  , [C 0 0, C 0 0]
  ]
pX 1 = fromJust . matrix $
  [
    [C 0 0, C 0 0]
  , [C 0 0, C 1 0]
  ]

pY 0 = fromJust . matrix $
  [
    [real $ 1%2, real $ -1%2]
  , [real $ -1%2, real $ 1%2]
  ]
pY 1 = fromJust . matrix $
  [
    [real $ 1%2, real $ 1%2]
  , [real $ 1%2, real $ 1%2]]

pZ 0 = fromJust . matrix $
  [
    [real $ 1%2, imag $ 1%2]
  , [imag $ -1%2, real $ 1%2]
  ]
pZ 1 = fromJust . matrix $
  [
    [real $ 1%2, imag $ -1%2]
  , [imag $ 1%2, real $ 1%2]
  ]

type family Composed (m :: Nat) (n :: * -> *) :: Nat where
  Composed m Two = m * m
  Composed m Three = m * m * m

atomRepr1 :: Box -> Matrix 2 (Complex Rational)
atomRepr1 (Box 'X' !i) = pX i
atomRepr1 (Box 'Y' !i) = pY i
atomRepr1 (Box 'Z' !i) = pZ i

-- atomRepr :: (System s, KnownNat n) => s Box -> Matrix (Composed 2 s) (Complex Rational)
-- -- atomRepr :: (System s, KnownNat n) => s Box -> s (Matrix 2 (Complex Rational))
-- atomRepr = foldl1 tensor . fmap atomRepr1

atomRepr2 :: Two Box -> Matrix 4 (Complex Rational)
atomRepr2 (Two (!a, !b)) = atomRepr1 a `tensor` atomRepr1 b

atomRepr3 :: Three Box -> Matrix 8 (Complex Rational)
atomRepr3 (Three (!a, !b, !c)) = atomRepr1 a `tensor` atomRepr1 b `tensor` atomRepr1 c

questionRepr1 (Question !atoms) = foldl1' (^+^) $ map atomRepr1 atoms
questionRepr2 (Question !atoms) = foldl1' (^+^) $ map atomRepr2 atoms
questionRepr3 (Question !atoms) = foldl1' (^+^) $ map atomRepr3 atoms

-- questionRepr :: (System s, Foldable s) => Question (s Box) -> Matrix n (Complex Rational)
-- questionRepr (Question atoms) = foldl1' (+) $ map atomRepr atoms
