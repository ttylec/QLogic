{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Data.QLogic.BoxWorldConcrete where

import Prelude hiding (null)
import Data.Maybe
import Data.List (nub)

import Data.QLogic --hiding (ConcretePoset)
-- import Data.QLogic.Examples
import Data.QLogic.Utils
import Data.Poset.Internals
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntSet (IntSet, isSubsetOf, empty, union, intersection, difference, fromList, null)

-- data ConcretePoset a where
--     ConcretePoset :: [Set a] -> ConcretePoset a
-- 
-- instance (Ord a) => POrdStruct (ConcretePoset a) (Set a) where
--     elementsOf (ConcretePoset els) = els
--     lessIn _  = isSubsetOf
--     supIn poset a b 
--         | intersection a b == empty = Just $ union a b
--         | length lub == 1 = Just $ head lub
--         | otherwise = Nothing
--           where
--               lub = lubIn poset a b

phaseSpace :: Ord a => [(a, Int)] -> [Map a Int]
phaseSpace [] = []
phaseSpace ((i, k):[]) = map (Map.singleton i) [0..k-1]
phaseSpace ((i, k):rest) = [Map.insert i o p | o <- [0..k-1], p <- phaseSpace rest]

data Observable = Observable String Int deriving (Eq, Ord)

instance Show Observable where
    show (Observable n _) = n

data Question = Question Observable Int | QProd Question Question | QPlus Question Question

(<>) :: Question -> Question -> Question
a <> b = QProd a b
infixl 5 <>

(<+>) :: Question -> Question -> Question
a <+> b = QPlus a b
infixl 4 <+>

instance Show Question where
   show (Question (Observable n _) v) = n ++ (show v)
   show (QProd q p) = (show q) ++ (show p)
   show (QPlus q p) = (show q) ++ "⊕" ++ (show p)

type BoxPhasePoint = Map Observable Int

questionsOf :: Observable -> [Question]
questionsOf obs@(Observable _ d) = map (Question obs) [0..d-1]

setRepr :: Question -> [BoxPhasePoint] -> [BoxPhasePoint]
setRepr (Question obs v) phase = filter ((v ==) . observableValue obs) $ phase
setRepr (QProd (Question obs v) p) phase = filter ((v ==) . observableValue obs) $ setRepr p phase
setRepr (QPlus q p) phase = (setRepr q phase) ++ (setRepr p phase)

observableValue :: Observable -> Map Observable Int -> Int
observableValue obs p = fromJust $ Map.lookup obs p

boxWorldPhaseSpace2 :: [Observable] -> [Observable] -> [Map Observable Int]
boxWorldPhaseSpace2 left right = phaseSpace $ (inputs left) ++ (inputs right)
    where
        inputs = map unwrap
        unwrap obs@(Observable _ k) = (obs, k)

boxWorldAtomicQs2 :: [Observable] -> [Observable] -> [Question]
boxWorldAtomicQs2 left right = [QProd qa qb | qa <- allQuestions left, qb <- allQuestions right]
    where
        allQuestions obs = concat $ map questionsOf obs

boxQRepr :: Packed BoxPhasePoint -> Question -> IntSet
boxQRepr pack q@(QProd _ _) = fromList $ map (toKey pack) $ setRepr q $ packedElements pack
boxQRepr pack q@(QPlus a b) = union (boxQRepr pack a) (boxQRepr pack b)

concreteLogic :: IntSet -> [IntSet] -> QLogic ConcretePoset IntSet
concreteLogic space els = fromPoset (ConcretePoset els) booleanOcmpl 
    where
        booleanOcmpl = difference space

boxWorldCLogic2 :: [Observable] -> [Observable] -> (Question -> IntSet, QLogic ConcretePoset IntSet)
boxWorldCLogic2 left right = (qrepr, concreteLogic space els)
    where
        els = nub $ generateConcreteElems atoms
        atoms = map (boxQRepr packedPhase) $ boxWorldAtomicQs2 left right 
        packedPhase = packList phase
        phase = boxWorldPhaseSpace2 left right
        space = fromList [0..length phase - 1] 
        qrepr = boxQRepr packedPhase

generateConcreteElems :: [IntSet] -> [IntSet]
generateConcreteElems [] = [empty]
generateConcreteElems (a:as) = generateConcreteElems as ++ 
                                    map (union a) (generateConcreteElems disjoint)
    where
        disjoint = filter (null . intersection a) as

askOnLeft :: Question -> [Observable] -> Question
askOnLeft q (left:_) = foldl1 (<+>) $ map (q <>) $ questionsOf left

askOnRight :: Question -> [Observable] -> Question
askOnRight q (left:_) = foldl1 (<+>) $ map (<> q) $ questionsOf left

