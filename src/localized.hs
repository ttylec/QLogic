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

localizedFirst :: (System s, Ord (s Point), Ord (s' Point), Splitting s s') =>
  BoxModel s -> Set (s Point) -> Bool
localizedFirst model q = all (== gamma') . map setFromSnd $ points
  where
    (PhaseSpace gamma') = phaseSpace . snd . split $ model
    points = groupBy firstEq . sortBy firstCmp . map split $ Set.toList q
    firstCmp (a, _) (b, _) = a `compare` b
    firstEq (a, _) (b, _) = a == b
    setFromSnd = Set.fromList . map snd

main :: IO ()
main = do
  let
    boxes = twoboxes
    -- boxes = threeboxes
    ea = boxWorldPropositions boxes concreteIntSubEA
    ea' = boxWorldPropositions' boxes concreteSubEA
    omp = boxWorldPropositions boxes concreteIntSubOMP
    oml = boxWorldPropositions boxes concreteIntSubOML

  -- print . length . elementsOf $ ea
  -- print . length . atomsOf $ ea
  -- print . length . elementsOf $ omp
  -- print . length . atomsOf $ omp

  -- let
  --   a0 = head . atomsOf $ ea
  --   atomsOMP = atomsOf omp

  -- print $ all (`elem` atomsOMP) . atomsOf $ ea
  -- print $ length $ atomicDecomposition omp (oneOf omp)
  -- print $ allStateRestrictionRank omp
  -- print $ nsrankValue . boxConstraints $ boxes

  print "----"
  print $ length . filter (localizedFirst boxes) . elementsOf $ ea'
