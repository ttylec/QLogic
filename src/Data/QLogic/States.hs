module Data.QLogic.States where

import Data.List
import System.Random

import Data.QLogic
import Data.QLogic.Utils

data State a v = State (a -> v)

data AtomicState p a v = AtomicState (QLogic p a) (a -> v)

evalState :: Real v => State a v -> a -> v
evalState (State rho) a = rho a 

fromAtomicState :: (Eq a, Real v) => AtomicState p a v -> State a v
fromAtomicState (AtomicState ql arho) = State rho
    where
        rho a = sum . map arho . head . atomicDecomposition ql $ a

isState :: Real v => QLogic p a -> State a v -> Bool
isState ql rho = isStateI ql rho && isStateII ql rho

isStateI ql rho = evalState rho (oneOf ql) == 1

isStateII ql rho = and $ map (\as -> lhs as == rhs as) $ subsetsBy (disjointIn ql) $ atomsOf ql 
    where
        lhs [] = 0
        lhs (a:as) = evalState rho $ foldl' (@+@) a as
        rhs as = sum $ map (evalState rho) as
        (@+@) = unsafeSupIn ql

simulateMeasurement :: (Real v, Random v, RandomGen g) => g -> State a v -> a -> [Bool]
simulateMeasurement g rho q = map (<= evalState rho q) $ randomRs (0, 1) g
