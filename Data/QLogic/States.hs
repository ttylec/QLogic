{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.QLogic.States where

import Data.List
import System.Random

import Data.QLogic
import Data.QLogic.Utils

data State a f = State {evalState :: a -> f}

fromAtomicState :: (Num f, QLogicStruct p a) => p -> (a -> f) -> State a f
fromAtomicState ql s = State $ sum . map s . head . atomicDecomposition ql

fromAtomicList :: (Eq a, Num f, QLogicStruct p a) => p -> [(a, f)] -> State a f 
fromAtomicList ql ls = fromAtomicState ql (lookup ls)
        where
            lookup qs q = case find ((q==) . fst) $ qs of
                Just (_, v) -> v
                Nothing     -> 0

isState :: (Eq f, Num f, QLogicStruct p a) => p -> State a f -> Bool
isState ql rho = isStateI ql rho && isStateII ql rho

isState' :: (Eq f, Num f, QLogicStruct p a) => p -> State a f -> Bool
isState' ql rho = isStateI ql rho && isStateII' ql rho

isStateI :: (Eq f, Num f, QLogicStruct p a) => p -> State a f -> Bool
isStateI ql rho = evalState rho (oneOf ql) == 1

isStateII' :: (Eq f, Num f, QLogicStruct p a) => p -> State a f -> Bool
isStateII' ql rho = and $ map ((1==) . rhs)  $ decomps
    where
        lhs [] = 0
        lhs (a:as) = evalState rho $ foldl' (\/) a as
        rhs as = sum $ map (evalState rho) as
        (\/) = unsafeSupIn ql
        decomps = atomicDecomposition ql $ oneOf ql

isStateII :: (Eq f, Num f, QLogicStruct p a) => p -> State a f -> Bool
isStateII ql rho = and $ map (\as -> lhs as == rhs as) $ subsetsBy (orthoIn ql) $ atomsOf ql 
    where
        lhs [] = 0
        lhs (a:as) = evalState rho $ foldl' (\/) a as
        rhs as = sum $ map (evalState rho) as
        (\/) = unsafeSupIn ql

-- 
-- simulateMeasurement :: (Real v, Random v, RandomGen g) => g -> State a v -> a -> [Bool]
-- simulateMeasurement g rho q = map (<= evalState rho q) $ randomRs (0, 1) g
