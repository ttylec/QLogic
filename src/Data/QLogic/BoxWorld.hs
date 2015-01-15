module Data.QLogic.BoxWorld where

import Data.List

import Data.QLogic
import Data.QLogic.BoxProduct
import Data.QLogic.BoxProduct (FreeProduct(FreeProd, FreePlus))
import Data.QLogic.Utils

import Data.LinearProgram
import Data.LinearProgram.GLPK

import Control.Monad.Trans.State.Strict (StateT)
import Data.Functor.Identity (Identity)

import Data.Monoid (mappend)

data Observable = Observable String [Int]

instance Eq Observable where
        (Observable na da) == (Observable nb db) = na == nb && da == db

instance Ord Observable where
        -- (Observable na da) `compare` (Observable nb db) = (na `compare` nb) `mappend` (da `compare` db)
        (Observable na da) `compare` (Observable nb db) = na `compare` nb

instance Show Observable where
        show (Observable name domain) = name ++ ": " ++ (show domain) 

data Question = Question Observable [Int]

instance Eq Question where
        (Question _ []) == (Question _ []) = True
        (Question obsa@(Observable _ adom) a) == (Question obsb@(Observable _ bdom) b) 
            | a == adom && b == bdom = True
            | otherwise = obsa == obsb && a == b

instance Ord Question where
        (Question oa a) `compare` (Question ob b) = {-# SCC qcompare #-} (oa `compare` ob) `mappend` (a `compare` b)

instance POrd Question where
        (Question oa a) .<=. (Question ob b) | oa /= ob = False
                                             | otherwise = a `isSubset` b

instance Show Question where
        show (Question _ []) = "Zero"
        show (Question (Observable name domain) a) 
            | a == domain = "One"
            | otherwise = name ++ (show a)

questionOf :: Observable -> [Int] -> Question
questionOf obs v = Question obs $ sort v

questionsOf :: Observable -> [Question]
questionsOf obs@(Observable _ domain) = map (questionOf obs) $ subsets domain

atomicQuestionsOf :: Observable -> [Question]
atomicQuestionsOf obs@(Observable _ domain) = map (questionOf obs) $ map (:[]) domain

boxLogic :: [Observable] -> QLogic Question
boxLogic os = fromPoset poset ortho
    where
        poset = fromPOrd els
        els = nub $ concat $ map questionsOf os
        ortho (Question obs@(Observable _ domain) a) = questionOf obs $ filter (not . (`elem` a)) domain

ssimplexDefaults = SimplexOpts MsgOff 10000 True

boxWorldLPVars :: [Question] -> [Question] -> [FreeProduct Question Question]
boxWorldLPVars as bs = [a <> b | a <- as, b <- bs]

normalization :: [[Question]] -> [[Question]] -> [LinFunc (FreeProduct Question Question) Int]
normalization as bs = map varSum [boxWorldLPVars a b | a <- as, b <- bs]

nonsignalling :: [[Question]] -> [[Question]] -> 
    [(LinFunc (FreeProduct Question Question) Int, LinFunc (FreeProduct Question Question) Int)]
nonsignalling as bs = lefts ++ rights
    where
        lefts = concat $ map (nsLeft bs) $ concat as
        rights = concat $ map (nsRight as) $ concat bs

nsLeft :: [[Question]] -> Question ->
    [(LinFunc (FreeProduct Question Question) Int, LinFunc (FreeProduct Question Question) Int)]
nsLeft bbs a = pairs [varSum [a <> b | b <- bs] | bs <- bbs]

nsRight :: [[Question]] -> Question ->
    [(LinFunc (FreeProduct Question Question) Int, LinFunc (FreeProduct Question Question) Int)]
nsRight aas b = pairs [varSum [a <> b | a <- as] | as <- aas]

-- | Objective function representing value on a state
objValue :: FreeProduct Question Question -> LinFunc (FreeProduct Question Question) Int
objValue a@(FreeProd _ _) = var a
objValue (FreePlus a as) = (var a) ^+^ objValue as

-- | Objective function used to compute order relation
objLess :: FreeProduct Question Question -> FreeProduct Question Question -> 
    LinFunc (FreeProduct Question Question) Int
objLess a b = (objValue a) ^-^ (objValue b)

isZero :: FreeProduct Question Question -> Bool
isZero (FreeProd (Question _ []) _) = True
isZero (FreeProd _ (Question _ [])) = True
isZero _ = False

isBWQuestion :: BWConstraints -> FreeProduct Question Question -> IO Bool
isBWQuestion constr a 
    | isZero a = return True
    | otherwise = do
        (_, Just (maxp, _)) <- glpSolveVars ssimplexDefaults $ boxWorldLogicSolver constr obj
        -- putStrLn $ show $ obj
        return (maxp <= 1.0)
        where
            obj = objValue a

isBWLess :: BWConstraints -> FreeProduct Question Question -> FreeProduct Question Question -> IO Bool
isBWLess constr a b 
    | isZero a = return True
    | isZero b = return False
    | otherwise = do
        (_, Just (maxp, _)) <- {-# SCC solver #-} glpSolveVars ssimplexDefaults $ boxWorldLogicSolver constr obj
        return (not $ maxp > 0.0)
        where
            obj = objLess a b

boxWorldLogicSolver :: BWConstraints -> LinFunc (FreeProduct Question Question) Int -> LP (FreeProduct Question Question) Int
boxWorldLogicSolver constr obj = execLPM $ do
    setDirection Max
    setObjective obj
    mapM_ (\f -> equalTo f 1) $ normConstr constr
    mapM_ (\(f, g) -> equal f g) $ nonsigConstr constr
    mapM_ (\v -> varGeq v 0) $ vars constr
    mapM_ (\v -> setVarKind v ContVar) $ vars constr


boxWorldConstraints :: [Observable] -> [Observable] -> BWConstraints
boxWorldConstraints obsa obsb = BWConstraints { normConstr = normalization as bs
                                              , nonsigConstr = nonsignalling as bs
                                              , vars = boxWorldLPVars (concat as) (concat bs) }
      where
          as = map questionsOf obsa
          bs = map questionsOf obsb

data BWConstraints = BWConstraints { normConstr :: [LinFunc (FreeProduct Question Question) Int]
                                   , nonsigConstr :: [(LinFunc (FreeProduct Question Question) Int, LinFunc (FreeProduct Question Question) Int)]
                                   , vars :: [FreeProduct Question Question] }

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a:as) = (map (\b -> (a, b)) as) ++ pairs as

