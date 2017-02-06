{-# LANGUAGE TypeInType #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import Data.Proxy
import Data.Maybe
import Data.Complex
import Linear.Matrix
import Linear.V
import Linear.Vector
-- import BoxModels.HilbertRepresentation
import QStructures.Hilbert
import qualified Data.Vector as V

v = fromJust . fromVector $ V.fromList [1, 2] :: V 2 Int
-- m = Matrix $ (fromJust . fromVector $ V.fromList [v, v] :: V 2 (V 2 Int))
-- m = matrix [[1, 2], [2, 1]] :: Maybe (V 2 (V 2 Int))
m = matrix [[1, 2], [2, 1, 2]] :: Maybe (Matrix 2 Int)

main :: IO ()
main = do
  print v
  print m
  -- print $ (pX 0 :: Matrix 2 (Complex Rational))
