{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QLogic.Concrete
       ( Concrete(..)
       , ConcreteInt(..)
       , booleanAlgebra
       , booleanAlgebraInt
       , concreteSublogic
       , concreteIntSublogic
       , concreteIntSublogic'
       ) where

import Data.Set (Set)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import QLogic
import QLogic.Utils

-- | A concrete poset
data Concrete a = Concrete (Set a) [Set a]

instance (Ord a) => POrdStruct (Concrete a) (Set a) where
    elementsOf (Concrete _ els) = els
    lessIn _  = Set.isSubsetOf
    supIn poset a b
        | Set.intersection a b == Set.empty = Just $ Set.union a b
        | length lub == 1 = Just $ head lub
        | otherwise = Nothing
          where
              lub = lubIn poset [a, b]

instance (Ord a) => QLogicStruct (Concrete a) (Set a) where
    ocmplIn ql      = Set.difference (oneOf ql)
    orthoIn _ a b   = Set.null $ Set.intersection a b
    compatIn ql a b = Set.intersection a b `elem` elementsOf ql
    zeroOf _ = Set.empty
    oneOf (Concrete space _) = space

instance (Ord a, Show a) => Show (Concrete a) where
        show poset = "No. of elements: " ++ (show . length $ elementsOf poset) ++
                     "\nGreater than lists:\n" ++ unlines gtlists
            where
                gtlists = map (\a -> show a ++ "| " ++ show (geEqThan poset a)) $ elementsOf poset

data ConcreteInt = ConcreteInt IntSet [IntSet]

instance POrdStruct ConcreteInt IntSet where
    elementsOf (ConcreteInt _ els) = els
    lessIn _  = IntSet.isSubsetOf
    supIn poset a b
        | IntSet.intersection a b == IntSet.empty = Just $ IntSet.union a b
        | length lub == 1 = Just $ head lub
        | otherwise = Nothing
          where
              lub = lubIn poset [a, b]

instance QLogicStruct ConcreteInt IntSet where
    ocmplIn ql      = IntSet.difference (oneOf ql)
    orthoIn _ a b   = IntSet.null $ IntSet.intersection a b
    compatIn ql a b = IntSet.intersection a b `elem` elementsOf ql
    zeroOf _ = IntSet.empty
    oneOf (ConcreteInt space _) = space

concreteIntSublogic :: ConcreteInt -> [IntSet] -> ConcreteInt
concreteIntSublogic ql = ConcreteInt (oneOf ql) . ordNub . subLogicElems ql
-- concreteIntSublogic (ConcreteInt space _) =
--   ConcreteInt space . ordNub . Set.toList . generateIntSublogic space . Set.fromList

concreteIntSublogic' :: ConcreteInt -> [IntSet] -> ConcreteInt
concreteIntSublogic' ql = ConcreteInt (oneOf ql) . ordNub . generateAtomicSubLogic ql

generateAtomicSubLogic :: (QLogicStruct p a, Ord a) => p -> [a] -> [a]
generateAtomicSubLogic ql atoms =
  Set.toList . generateAtomicSubLogic' ql atoms . Set.fromList $ atoms

generateAtomicSubLogic' :: (QLogicStruct p a, Ord a) => p -> [a] -> Set a -> Set a
generateAtomicSubLogic' ql atoms accum
  | Set.size accum == Set.size next = accum
  | otherwise = generateAtomicSubLogic' ql atoms next
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

generateIntSublogic :: IntSet -> Set IntSet -> Set IntSet
generateIntSublogic space accum
  | Set.size accum == Set.size next = accum
  | otherwise = generateIntSublogic space next
  where
    next = accum `Set.union` complements `Set.union`
           (Set.fromList . sums . Set.toList $ accum)
    complements = Set.map (space `IntSet.difference`) accum
    sums [] = []
    sums (a:as) = sums' as ++ sums as
      where
        sums' = map (a `IntSet.union`) .
                     filter (IntSet.null . (a `IntSet.intersection`))

concreteSublogic :: Ord a => Concrete a -> [Set a] -> Concrete a
concreteSublogic ql = Concrete (oneOf ql) . ordNub . subLogicElems ql

booleanAlgebra :: Ord a => Set a -> Concrete a
booleanAlgebra space = Concrete space (map Set.fromList . subsets . Set.elems $ space)

booleanAlgebraInt :: IntSet -> ConcreteInt
booleanAlgebraInt space = ConcreteInt space (map IntSet.fromList . subsets . IntSet.elems $ space)


subLogicElems :: (QLogicStruct p a) => p -> [a] -> [a]
subLogicElems ql [] = [zeroOf ql]
subLogicElems ql (a:as) = subLogicElems ql as ++ map (a \/) (subLogicElems ql disj)
    where
        (\/) = unsafeSupIn ql
        disj = filter (orthoIn ql a) as

atomicSubLogicElems :: (QLogicStruct p a) => p -> [a] -> [a] -> [a]
atomicSubLogicElems ql atoms [] = [zeroOf ql]
atomicSubLogicElems ql atoms (a:as) =
  subLogicElems ql as ++
  (filter goodone . map (a \/) $ subLogicElems ql disj)
    where
        goodone = not . null . decomposeInto ql atoms . ocmplIn ql
        (\/) = unsafeSupIn ql
        disj = filter (orthoIn ql a) as

ordNub :: (Ord a) => [a] -> [a]
ordNub = Set.toList . Set.fromList
