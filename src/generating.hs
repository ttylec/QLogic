{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Environment

import Control.Arrow

import QLogic
import QLogic.GeneralBoxes
import QLogic.BoxWorld
import QLogic.Concrete
import QLogic.Utils

import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.LinearProgram hiding (constraints, one)
import Numeric.LinearAlgebra

allStateRestrictionRank :: (Ord a, QLogicStruct p a) => p -> Int
allStateRestrictionRank ql = rank . coefficientMatrix vars $ restr
  where
    vars = Map.fromList $ zip (atomsOf ql) [0..]
    restr = map (uncurry (^-^) . (toLin *** toLin)) . pairs . atomicDecomposition ql
            $ oneOf ql
    toLin as = varSum as

main :: IO ()
main = do
  let
    -- boxes = twoboxes
    boxes = threeboxes
    ea = boxWorldPropositions'' boxes concreteIntSubEA
    -- ea' = boxWorldPropositions' boxes concreteSubEA
    -- omp = boxWorldPropositions boxes concreteIntSubOMP
    -- oml = boxWorldPropositions boxes concreteIntSubOML

  print . length . elementsOf $ ea
  print . length . atomsOf $ ea
  -- print . length . elementsOf $ omp
  -- print . length . atomsOf $ omp

  -- let
  --   a0 = head . atomsOf $ ea
  --   atomsOMP = atomsOf omp

  -- print $ all (`elem` atomsOMP) . atomsOf $ ea
  -- print $ length $ atomicDecomposition omp (oneOf omp)
  -- print $ allStateRestrictionRank omp
  -- print $ nsrankValue . boxConstraints $ boxes
