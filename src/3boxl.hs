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

-- boxWorldALogic :: (System s, Ord (s Point), Ord (s Box))
--               => BoxModel s
--               -> Representation ConcreteInt IntSet (Question (s Box))
boxWorldALogic obs = Representation q2set set2q ql
  where
    ql        = concreteIntSublogic' (booleanAlgebraInt space) (map q2set atomicQs)
    phase     = phaseSpace obs
    atomicQs  = boxAtoms obs
    phasePack = packList . phasePoints $ phase
    packSet   = IS.fromList . map (toKey phasePack) . Set.toList
    space     = IS.fromList [0..length atomicQs - 1]
    q2set     = packSet . phaseSubset phase
    invAtoms  = rightInvMap atomicQs q2set
    set2q q
        | IS.null q = nullQ
        | otherwise = collapse . sequenceA . map (invAtoms Map.!) . decompose ql $ q
            where
              collapse (Question as) = Question . concat $ as

main :: IO ()
main = do
  let
      boxWorld = boxWorldLogic threeboxes
      boxWorldA= boxWorldALogic threeboxes
      boxWorldRA= logicRepr boxWorldA
      boxWorldR = logicRepr boxWorld
      constr = boxConstraints threeboxes
      threeBoxElems = filter ((<= 1) . maxStateValue constr)
                      . elementsOf $ boxWorld

  let
    gyni1 = read "[X0X0X0]" ::  Question (Three Box)
    gyni2 = read "[X1Y1Y0]" ::  Question (Three Box)
    gyni3 = read "[Y0X1Y1]" ::  Question (Three Box)
    gyni4 = read "[Y1Y0X1]" ::  Question (Three Box)


  -- print $ summable constr gyni1 gyni2
  -- print $ summable constr (gyni1 .@. gyni2) gyni3
  -- print $ summable constr (gyni1 .@. gyni2 .@. gyni3) gyni4

  let
    gyni123 = gyni1 .@. gyni2 .@. gyni3
    gyni = gyni1 .@. gyni2 .@. gyni3 .@. gyni4

  print "Elements of logic:"
  print $ length . elementsOf $ boxWorld
  print $ length . elementsOf $ boxWorldA
  print $ length threeBoxElems

  let
    strange = filter ((>1) . maxStateValue constr) . elementsOf $ boxWorldA

  print strange
  print $ map (maxStateValue constr) strange
  print $ map (ocmplIn boxWorldA) strange

  -- print $ decomposeInto boxWorldRA (atomsOf boxWorldRA) . toRepr boxWorldA $ gyni
  -- print $ decomposeInto boxWorldRA (atomsOf boxWorldRA) .
  --   ocmplIn boxWorldRA . toRepr boxWorldA $ gyni
  -- -- print $ isLogic . logicRepr $ boxWorld
  -- -- print $ length threeBoxElems
  -- print "Orthogonality of gyni components"
  -- print $ orthoIn boxWorld gyni1 gyni2
  -- print $ orthoIn boxWorld (gyni1 .@. gyni2) gyni3
  -- print $ orthoIn boxWorld (gyni1 .@. gyni2 .@. gyni3) gyni4

  -- print "Whether gyni exists and what is its complement"
  -- print $ toRepr boxWorld gyni `elem` elementsOf (logicRepr boxWorld)
  -- print $ fromRepr boxWorld . toRepr boxWorld $ gyni
  -- print $ fromRepr boxWorld . ocmplIn boxWorldR . toRepr boxWorld $ gyni
  -- print $ ocmplIn boxWorldR . toRepr boxWorld $ gyni

  -- let gynic = ocmplIn boxWorldR . toRepr boxWorld $ gyni
  -- print $ filter (\a -> lessIn boxWorldR a gynic) $ atomsOf boxWorldR
  -- print $ atomsOf boxWorldR

  -- print $ ocmplIn boxWorld gyni `elem` elementsOf boxWorld



  -- print $ strongDisjoint (toRepr boxWorld gyni123) (toRepr boxWorld gyni4)
  -- print $ take (read n) . elementsOf $ boxWorld
  -- -- print $ take (read n) . elementsOf . logicRepr $ boxWorld
  -- print $ length threeBoxElems
