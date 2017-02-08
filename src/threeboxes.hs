{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main (main) where

import Data.List
import qualified Data.Text as T
import System.Environment

import QStructures
import QStructures.Concrete
import QStructures.Hilbert
import BoxModels
import BoxModels.SetRepresentation
import BoxModels.HilbertRepresentation

model = let x = binaryO 'X'
            y = binaryO 'Y'
            z = binaryO 'Z'
        in
          three [x, y] [x, y] [x, y]

main :: IO ()
main = do
  let
    generators = map questionRepr3 . boxAtoms $ model
    ea' = generateSubStructure . HilbertBoxEA 8 generators $ generators
    omp' = generateSubStructure . HilbertOMP 8 $ generators
    ea = HilbertBoxEA 8 generators ea'
    omp = HilbertOMP 8 omp'

  putStrLn "Generating BoxEA"
  putStrLn "================"

  putStrLn $ "Number of generators: " ++ (show . length $ generators)
  putStrLn $ "Number of elements: " ++ (show . length $ ea')
  putStrLn $ "Number of atoms: " ++ (show . length . atomsOf $ ea)

  -- putStrLn "Generating OMP"
  -- putStrLn "================"

  -- putStrLn $ "Number of generators " ++ (show . length $ generators)
  -- putStrLn $ "Number of elements: " ++ (show . length $ omp')
  -- putStrLn $ "Number of atoms " ++ (show . length . atomsOf $ omp)
