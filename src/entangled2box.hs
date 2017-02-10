{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main (main) where

import Data.List
import qualified Data.Text as T
import System.Environment
import Data.Ratio
import Data.Maybe

import QStructures
import QStructures.Concrete
import QStructures.Hilbert
import BoxModels
import BoxModels.SetRepresentation
import BoxModels.HilbertRepresentation

bell1 = fromJust . matrix $
  [
    [real $ 1%2,  0,  0, real $ 1%2]
  , [0,  0,  0,  0]
  , [0,  0,  0,  0]
  , [real $ 1%2,  0,  0, real $ 1%2]
  ] :: Matrix 4 (Complex Rational)

bell2 = fromJust . matrix $
  [
    [real $ 1%2,  0,  0, real $ -1%2]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [real $ -1%2,  0,  0, real $ 1%2]
  ] :: Matrix 4 (Complex Rational)

bell3 = fromJust . matrix $
  [
    [0, 0, 0, 0]
  , [0, real $ 1%2, real $ 1%2,  0]
  , [0, real $ 1%2, real $ 1%2,  0]
  , [0 , 0, 0, 0]
  ] :: Matrix 4 (Complex Rational)

bell4 = fromJust . matrix $
  [
    [0, 0, 0, 0]
  , [0, real $ 1%2, real $ -1%2,  0]
  , [0, real $ -1%2, real $ 1%2,  0]
  , [0 , 0, 0, 0]
  ] :: Matrix 4 (Complex Rational)

model = let x = binaryO 'X'
            y = binaryO 'Y'
            z = binaryO 'Z'
        in
          two [x, y, z] [x, y, z]

main :: IO ()
main = do
  let
    generators = bell1:bell2:bell3:bell4:(map questionRepr2 . boxAtoms $ model)
    ea' = generateSubStructure . HilbertBoxEA 4 generators $ generators
    omp' = generateSubStructure . HilbertOMP 4 $ generators
    ea = HilbertBoxEA 4 generators ea'
    omp = HilbertOMP 4 omp'

  putStrLn "Generating BoxEA"
  putStrLn "================"

  putStrLn $ "Number of generators: " ++ (show . length $ generators)
  putStrLn $ "Number of elements: " ++ (show . length $ ea')
  putStrLn $ "Number of atoms: " ++ (show . length . atomsOf $ ea)

  putStrLn "Generating OMP"
  putStrLn "================"

  putStrLn $ "Number of generators " ++ (show . length $ generators)
  putStrLn $ "Number of elements: " ++ (show . length $ omp')
  putStrLn $ "Number of atoms " ++ (show . length . atomsOf $ omp)
