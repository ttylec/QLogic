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

localizedOther :: (System s, Ord (s Point), Ord (s' Point), Splitting s s') =>
  BoxModel s -> Set (s Point) -> Bool
localizedOther model q = all (== gamma') . map setFromSnd $ points
  where
    (PhaseSpace gamma') = phaseSpace . fst . split $ model
    points = groupBy secondEq . sortBy secondCmp . map split $ Set.toList q
    secondCmp (_, a) (_, b) = a `compare` b
    secondEq (_, a) (_, b) = a == b
    setFromSnd = Set.fromList . map fst

main :: IO ()
main = do
  let
    -- boxes = twoboxes
    boxes = threeboxes
    ea = boxWorldPropositions'' boxes concreteIntSubEA
    -- ea' = boxWorldPropositions' boxes concreteSubEA
    omp = boxWorldPropositions'' boxes concreteIntSubOMP
    -- oml = boxWorldPropositions boxes concreteIntSubOML

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

  putStrLn "Localized propositions in the effect algebra:"
  print $ length . filter (localizedFirst boxes) . elementsOf $ ea
  putStrLn "Localized propositions in the OMP:"
  print $ length . filter (localizedFirst boxes) . elementsOf $ omp

  putStrLn "Localized propositions in the effect algebra (rest):"
  print $ length . filter (localizedOther boxes) . elementsOf $ ea
  putStrLn "Localized propositions in the OMP (rest):"
  print $ length . filter (localizedOther boxes) . elementsOf $ omp
