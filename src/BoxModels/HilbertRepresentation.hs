{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module BoxModels.HilbertRepresentation where

import Data.List
import Data.Complex
import Data.Ratio
import Data.Foldable hiding (toList)

import QStructures.Hilbert
import BoxModels
import Linear.V
import qualified Data.Vector as V

-- tolerance = 1e-10

-- (~==) :: Mat -> Mat -> Bool
-- a ~== b = all (almostEq 0) . toList . flatten $ a - b
--   where
--     almostEq x y = magnitude (x - y) < tolerance

-- infixr 1 ~==

cI = 0 :+ 1 :: Complex Rational

-- e :: Int -> Int -> Vec
-- e dim i = dim |> ((take i $ repeat 0) ++ [1] ++ repeat 0)

-- proj :: Vec -> Mat
-- proj x = outer (conj nx) nx
--   where
--     nx = normalize x

-- normalize :: Vec -> Vec
-- normalize v = scale (1 / norm_2 v :+ 0) v

-- Binary box representation

matrix :: [[a]] -> Matrix n a
matrix ls = Matrix . V . V.fromList $ map (V . V.fromList) ls

pX 0 = matrix [[1 :+ 0, 0 :+ 0], [0 :+ 0, 0 :+ 0]]
pX 1 = matrix [[0 :+ 0, 0 :+ 0], [0 :+ 0, 1 :+ 0]]

pY 0 = matrix [[1%2 :+ 0, (-1%2) :+ 0], [(-1%2) :+ 0, 1%2 :+ 0]]
pY 1 = matrix [[1%2 :+ 0,  1%2 :+ 0], [1%2 :+ 0,  1%2 :+ 0]]

pZ :: Int -> Matrix n (Complex Rational)
pZ 0 = matrix [[1%2 :+ 0, (-1%2) :+ 0], [(-1%2) :+ 0, 1%2 :+ 0]]
pZ 1 = matrix [[1%2 :+ 0, 0 :+ 1%2], [0 :+ (-1%2), 1%2 :+ 0]]

-- e2 i = e 2 i

-- x i = e2 i

-- y 0 = e2 0 + e2 1
-- y 1 = e2 0 - e2 1

-- z 0 = e2 0 - scale cI (e2 1)
-- z 1 = e2 0 + scale cI (e2 1)

atomRepr1 :: Box -> Matrix n (Complex Rational)
atomRepr1 (Box 'X' i) = pX i
atomRepr1 (Box 'Y' i) = pY i
atomRepr1 (Box 'Z' i) = pZ i

-- atomRepr :: (System s, Foldable s) => s Box -> Matrix n (Complex Rational)
-- atomRepr box = foldl1 kronecker $ fmap atomRepr1 box

-- questionRepr :: (System s, Foldable s) => Question (s Box) -> Matrix n (Complex Rational)
-- questionRepr (Question atoms) = foldl1' (+) $ map atomRepr atoms
