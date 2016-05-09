{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QLogic.Concrete
       ( Concrete(..)
       , ConcreteInt(..)
       , booleanAlgebraInt
       , concreteIntSublogic
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
        show poset = "No. of elements: " ++ (show $ length $ elementsOf poset) ++
                     "\nGreater than lists:\n" ++ (unlines gtlists)
            where
                gtlists = map (\a -> show a ++ "| " ++ show (geEqThan poset a)) $ elementsOf poset

data ConcreteInt = ConcreteInt IntSet [IntSet]

instance POrdStruct ConcreteInt IntSet where
    elementsOf (ConcreteInt space els) = els
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

booleanAlgebraInt :: IntSet -> ConcreteInt
booleanAlgebraInt space = ConcreteInt space (map IntSet.fromList . subsets . IntSet.elems $ space)


subLogicElems :: (QLogicStruct p a) => p -> [a] -> [a]
subLogicElems ql [] = [zeroOf ql]
subLogicElems ql (a:as) = subLogicElems ql as ++ map (a \/) (subLogicElems ql disj)
    where
        (\/) = unsafeSupIn ql
        disj = filter (orthoIn ql a) as

ordNub :: (Ord a) => [a] -> [a]
ordNub = Set.toList . Set.fromList
