module Main (main) where

import System.Environment

import QLogic
import QLogic.GeneralBoxes
import QLogic.BoxWorld
import QLogic.Concrete
import QLogic.Utils

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


concreteIntSubOML :: ConcreteInt -> [IntSet] -> ConcreteInt
concreteIntSubOML ql =  concreteSubInt ql generateOML

concreteIntSubOMP :: ConcreteInt -> [IntSet] -> ConcreteInt
concreteIntSubOMP ql = concreteSubInt ql generateOMP

concreteIntSubEA :: ConcreteInt -> [IntSet] -> ConcreteInt
concreteIntSubEA ql = concreteSubInt ql generateEA

concreteSubOML :: Ord a => Concrete a -> [Set a] -> Concrete a
concreteSubOML ql = concreteSub ql generateOML

concreteSubOMP :: Ord a => Concrete a -> [Set a] -> Concrete a
concreteSubOMP ql = concreteSub ql generateOMP

concreteSubEA :: Ord a => Concrete a -> [Set a] -> Concrete a
concreteSubEA ql = concreteSub ql generateEA

concreteSubInt ::
  ConcreteInt -> (ConcreteInt -> [IntSet] -> Set (IntSet) -> Set (IntSet))
  -> [IntSet] -> ConcreteInt
concreteSubInt ql generator atoms = ConcreteInt (oneOf ql) elements
  where
    elements = Set.toList . generator ql atoms . Set.fromList $ atoms

concreteSub :: Ord a =>
  Concrete a -> (Concrete a -> [Set a] -> Set (Set a) -> Set (Set a))
  -> [Set a] -> Concrete a
concreteSub ql generator atoms = Concrete (oneOf ql) elements
  where
    elements = Set.toList . generator ql atoms . Set.fromList $ atoms

generateOML :: (QLogicStruct p a, Ord a) => p -> [a] -> Set a -> Set a
generateOML ql atoms accum
  | Set.size accum == Set.size next = accum
  | otherwise = generateOML ql atoms next
  where
    next = accum `Set.union` complements `Set.union` sums
    complements = Set.map (ocmplIn ql) accum
    sums = Set.fromList . sums' . Set.toList $ accum
    sums' [] = []
    sums' (a:as) = go as ++ sums' as
      where
        go = map (a \/)
    (\/) = unsafeSupIn ql
    ortho = orthoIn ql

generateOMP :: (QLogicStruct p a, Ord a) => p -> [a] -> Set a -> Set a
generateOMP ql atoms accum
  | Set.size accum == Set.size next = accum
  | otherwise = generateOMP ql atoms next
  where
    next = accum `Set.union` complements `Set.union` sums
    complements = Set.map (ocmplIn ql) accum
    sums = Set.fromList . sums' . Set.toList $ accum
    sums' [] = []
    sums' (a:as) = go as ++ sums' as
      where
        go = map (a \/) . filter (`ortho` a)
    (\/) = unsafeSupIn ql
    ortho = orthoIn ql

generateEA :: (QLogicStruct p a, Ord a) => p -> [a] -> Set a -> Set a
generateEA ql atoms accum
  | Set.size accum == Set.size next = accum
  | otherwise = generateEA ql atoms next
  where
    next = accum `Set.union` complements `Set.union` sums
    complements = Set.map (ocmplIn ql) accum
    sums = Set.fromList . sums' . Set.toList $ accum
    sums' [] = []
    sums' (a:as) = go as ++ sums' as
      where
        go = filter (not . null . decomposeInto ql atoms . ocmplIn ql)
             . map (a \/) . filter (`ortho` a)
    (\/) = unsafeSupIn ql
    ortho = orthoIn ql

boxWorldPropositions :: (System s, Ord (s Point)) =>
  BoxModel s -> (ConcreteInt -> [IntSet] -> ConcreteInt) -> ConcreteInt
boxWorldPropositions obs typ = typ (booleanAlgebraInt space) (map q2set atomicQs)
  where
    atomicQs  = boxAtoms obs
    space     = IS.fromList [0..length atomicQs - 1]
    phase     = phaseSpace obs
    q2set     = packSet . phaseSubset phase
    packSet   = IS.fromList . map (toKey phasePack) . Set.toList
    phasePack = packList . phasePoints $ phase

boxWorldPropositions' :: (System s, Ord (s Point)) =>
  BoxModel s -> (Concrete (s Point) -> [Set (s Point)] -> Concrete (s Point))
  -> Concrete (s Point)
boxWorldPropositions' obs typ = typ (booleanAlgebra ps) (map q2set atomicQs)
  where
    phase@(PhaseSpace ps)= phaseSpace obs
    atomicQs  = boxAtoms obs
    q2set     = phaseSubset phase

main :: IO ()
main = do
  let
    ea = boxWorldPropositions threeboxes concreteIntSubEA
    omp = boxWorldPropositions threeboxes concreteIntSubOMP
    oml = boxWorldPropositions threeboxes concreteIntSubOML

  print . length . elementsOf $ ea
  print . length . elementsOf $ omp
  print . length . elementsOf $ oml
