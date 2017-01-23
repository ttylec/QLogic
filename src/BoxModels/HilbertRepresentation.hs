{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module BoxModels.HilbertRepresentation where

import Data.List
import Data.Foldable hiding (toList)
import Numeric.LinearAlgebra

import QStructures.Hilbert
import BoxModels hiding (x, y, z)


type Vec = Vector (Complex Double)
type Mat = Matrix (Complex Double)

tolerance = 1e-10

(~==) :: Mat -> Mat -> Bool
a ~== b = all (almostEq 0) . toList . flatten $ a - b
  where
    almostEq x y = magnitude (x - y) < tolerance

infixr 1 ~==

cI = 0 :+ 1

e :: Int -> Int -> Vec
e dim i = dim |> ((take i $ repeat 0) ++ [1] ++ repeat 0)

proj :: Vec -> Mat
proj x = outer (conj nx) nx
  where
    nx = normalize x

normalize :: Vec -> Vec
normalize v = scale (1 / norm_2 v :+ 0) v

-- Binary box representation

e2 i = e 2 i

x i = e2 i

y 0 = e2 0 + e2 1
y 1 = e2 0 - e2 1

z 0 = e2 0 - scale cI (e2 1)
z 1 = e2 0 + scale cI (e2 1)

atomRepr1 :: Box -> Mat
atomRepr1 (Box 'X' i) = proj $ x i
atomRepr1 (Box 'Y' i) = proj $ y i
atomRepr1 (Box 'Z' i) = proj $ z i

atomRepr :: (System s, Foldable s) => s Box -> Mat
atomRepr box = foldl1 kronecker $ fmap atomRepr1 box

questionRepr :: (System s, Foldable s) => Question (s Box) -> Mat
questionRepr (Question atoms) = foldl1' (+) $ map atomRepr atoms
