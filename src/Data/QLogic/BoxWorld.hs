module Data.QLogic.BoxWorld where

import Data.List

import Data.QLogic
import Data.QLogic.BoxProduct
import Data.QLogic.BoxProduct (FreeProduct(FreeProd, FreePlus))
import Data.QLogic.Utils
import Data.QLogic.IO

import Data.Poset.Internals
import Numeric.LinearProgramming
import qualified Data.Vector.Unboxed as V

import Data.LinearProgram
import Data.LinearProgram.GLPK

import Control.Monad.Trans.State.Strict (StateT)
import Data.Functor.Identity (Identity)

import Control.Monad.State.Class

import Data.Monoid (mappend)

data Observable = Observable String [Int]

instance Eq Observable where
        (Observable na da) == (Observable nb db) = na == nb && da == db

instance Ord Observable where
        (Observable na da) `compare` (Observable nb db) = (na `compare` nb) `mappend` (da `compare` db)

instance Show Observable where
        show (Observable name domain) = name ++ ": " ++ (show domain) 

data Question = Question Observable [Int] | NullQuestion | TrivialQuestion

instance Eq Question where
        NullQuestion == NullQuestion = True
        NullQuestion == (Question _ []) = True
        (Question _ []) == NullQuestion = True
        NullQuestion == _ = False
        _ == NullQuestion = False
        (Question _ []) == (Question _ []) = True
        (Question obsa@(Observable _ adom) a) == (Question obsb@(Observable _ bdom) b) 
            | a == adom && b == bdom = True
            | otherwise = obsa == obsb && a == b

instance Ord Question where
        (Question oa a) `compare` (Question ob b) = (oa `compare` ob) `mappend` (a `compare` b)

instance POrd Question where
        NullQuestion .<=. _ = True
        _ .<=. NullQuestion = False
        (Question oa a) .<=. (Question ob b) | oa /= ob = False
                                             | otherwise = a `isSubset` b

instance Show Question where
        show (Question _ []) = "Zero"
        show (Question (Observable name domain) a) 
            | a == domain = "One"
            | otherwise = name ++ (show a)

instance Repr Question where
        repr NullQuestion = "Zero"
        repr (Question (Observable name _) []) = "Zero"
        repr (Question (Observable name dom) vs) 
            | vs == dom = "One"
            | otherwise = name ++ (concat $ map show vs)

questionOf :: Observable -> [Int] -> Question
questionOf obs v = Question obs $ sort v

questionsOf :: Observable -> [Question]
questionsOf obs@(Observable _ domain) = map (questionOf obs) $ subsets domain

atomicQuestionsOf :: Observable -> [Question]
atomicQuestionsOf obs@(Observable _ domain) = map (questionOf obs) $ map (:[]) domain

boxLogic :: [Observable] -> QLogic (Poset Question) Question
boxLogic os = fromPoset poset ortho
    where
        poset = fromPOrd els
        els = nub $ concat $ map questionsOf os
        ortho (Question obs@(Observable _ domain) a) = questionOf obs $ filter (not . (`elem` a)) domain

data BoxWorld = BoxWorld { leftBox :: [[Question]]
                         , rightBox :: [[Question]]
                         , twoBoxes :: Packed (FreeProduct Question Question)} deriving (Show)

boxWorld :: [Observable] -> [Observable] -> BoxWorld
boxWorld as bs = BoxWorld { leftBox = lbox 
                          , rightBox = rbox 
                          , twoBoxes = packList atoms }
                          where
                              lbox = map atomicQuestionsOf as
                              rbox = map atomicQuestionsOf bs
                              atoms = [a <> b | a <- concat lbox, b <- concat rbox]

sumWith :: BoxWorld -> Double -> [FreeProduct Question Question] -> [(Double, Int)]
sumWith boxes c qs = zip (repeat c) $ map (toKey $ twoBoxes boxes) qs

boxStateOn :: BoxWorld -> FreeProduct Question Question -> [(Double, Int)]
boxStateOn boxes a@(FreeProd _ _) = [(1.0, 1 + toKey (twoBoxes boxes) a)]
boxStateOn boxes (FreePlus a as) = (1.0, 1 + toKey (twoBoxes boxes) a):(boxStateOn boxes as)

(.-.) :: [(Double, Int)] -> [(Double, Int)] -> [(Double, Int)]
a .-. b = a ++ (neg b)
    where
        neg = map (\(c, i) -> ((-1.0) * c, i))

qsum :: (Ord a, Ord b) => [FreeProduct a b] -> FreeProduct a b
qsum (a@(FreeProd _ _):[]) = a
qsum (a:as) = foldl' (<+>) a as

normalizationBounds :: BoxWorld -> [Bound [(Double, Int)]]
normalizationBounds boxes = [(boxStateOn boxes $ ones a b) :==: 1.0 | a <- leftBox boxes, b <- rightBox boxes]
    where
        ones as bs = qsum [a <> b | a <- as, b <- bs]

equalWith :: BoxWorld -> (FreeProduct Question Question, FreeProduct Question Question) -> Bound [(Double, Int)]
equalWith boxes (a, b) = lhs :==: 0.0
    where
        lhs = (boxStateOn boxes a) .-. (boxStateOn boxes b)

nonsignallingBound :: BoxWorld -> [Bound [(Double, Int)]]
nonsignallingBound boxes = (map (equalWith boxes) lefts) ++ (map (equalWith boxes) rights)
    where
        lefts = concat $ map (nsOnLeft bs) $ concat as
        rights = concat $ map (nsOnRight as) $ concat bs
        as = leftBox boxes
        bs = rightBox boxes

hsBoxWorldConstraints boxes = Sparse (normalizationBounds boxes ++ nonsignallingBound boxes)

printBound' :: BoxWorld -> [(Double, Int)] -> String
printBound' boxes bs = intercalate " + " $ map (\(c, i) -> (printCoeff c) ++ (show $ fromKey p (i - 1))) bs
    where
        printCoeff 1.0 = ""
        printCoeff v = (show v) ++ "*" 
        p = twoBoxes boxes

printBound :: BoxWorld -> Bound [(Double, Int)] -> String
printBound boxes (bs :<=: b) = printBound' boxes bs ++ " <= " ++ show b
printBound boxes (bs :==: b) = printBound' boxes bs ++ " == " ++ show b

printBounds boxes (Sparse bds) = unlines $ map (printBound boxes) bds

hsIsQ boxes constr q 
    | q == NullQuestion <> NullQuestion = True
    | otherwise = case simplex prob constr [] of
                      Optimal (p, _) -> p <= 1.0
                      otherwise -> False
                      where
                          prob = Maximize $ toDense n $ boxStateOn boxes q
                          n = 1 + (packedLength $ twoBoxes boxes)

hsIsQ' boxes constr q 
    | q == NullQuestion <> NullQuestion = Nothing
    | otherwise = Just $ simplex prob constr [] 
    where
        prob = Maximize $ toDense n $ boxStateOn boxes q
        n = 1 + (packedLength $ twoBoxes boxes)

hsIsLess boxes constr a b
    | a == z = True
    | b == z = False
    | otherwise = case simplex prob constr [] of
                      Optimal (p, _) -> p <= 0.0
                      otherwise -> False
                      where
                          prob = Maximize $ toDense n $ (boxStateOn boxes a .-. boxStateOn boxes b)
                          n = 1 + (packedLength $ twoBoxes boxes)
                          z = NullQuestion <> NullQuestion

toDense :: Int -> [(Double, Int)] -> [Double]
toDense n a = V.toList $ V.accum (+) (V.replicate n 0.0) $ map (\(v, i) -> (i - 1, v)) a

-- nsLeft :: [[Questions]] -> Question -> ound [(Double, Int)]
nsOnLeft bbs a = pairs [qsum [a <> b | b <- bs] | bs <- bbs]
nsOnRight aas b = pairs [qsum [a <> b | a <- as] | as <- aas]

ssimplexDefaults = SimplexOpts MsgOff 10000 True

boxWorldLPVars :: (Ord a, Ord b) => [a] -> [b] -> [FreeProduct a b]
boxWorldLPVars as bs = [a <> b | a <- as, b <- bs]

normalization :: (Ord a, Ord b) => [[a]] -> [[b]] -> [LinFunc (FreeProduct a b) Int]
normalization as bs = map varSum [boxWorldLPVars a b | a <- as, b <- bs]

nonsignalling :: (Ord a, Ord b) => [[a]] -> [[b]] -> 
    [(LinFunc (FreeProduct a b) Int, LinFunc (FreeProduct a b) Int)]
nonsignalling as bs = lefts ++ rights
    where
        lefts = concat $ map (nsLeft bs) $ concat as
        rights = concat $ map (nsRight as) $ concat bs

nsLeft :: (Ord a, Ord b) => [[b]] -> a ->
    [(LinFunc (FreeProduct a b) Int, LinFunc (FreeProduct a b) Int)]
nsLeft bbs a = pairs [varSum [a <> b | b <- bs] | bs <- bbs]

nsRight :: (Ord a, Ord b) => [[a]] -> b ->
    [(LinFunc (FreeProduct a b) Int, LinFunc (FreeProduct a b) Int)]
nsRight aas b = pairs [varSum [a <> b | a <- as] | as <- aas]

-- | Objective function representing value on a state
objValue :: (Ord a, Ord b) => FreeProduct a b -> LinFunc (FreeProduct a b) Int
objValue a@(FreeProd _ _) = var a
objValue (FreePlus a as) = (var a) ^+^ objValue as

-- | Objective function used to compute order relation
objLess :: (Ord a, Ord b) => FreeProduct a b -> FreeProduct a b -> LinFunc (FreeProduct a b) Int
objLess a b = (objValue a) ^-^ (objValue b)

isBWQuestion :: (Ord b, Ord a) => BWConstraints a b -> FreeProduct a b -> IO Bool
isBWQuestion constr a 
        | a == zeroElem constr = return True
        | otherwise = do
            (_, Just (maxp, _)) <- glpSolveVars ssimplexDefaults $ boxWorldLogicSolver constr obj
            -- putStrLn $ show $ obj
            return (maxp <= 1.0)
            where
                obj = objValue a

isBWLess :: (Ord a, Ord b) => BWConstraints a b -> FreeProduct a b -> FreeProduct a b -> IO Bool
isBWLess constr a b 
        | a == zeroElem constr = return True
        | b == zeroElem constr = return False
        | otherwise = do
            (_, Just (maxp, _)) <- {-# SCC solver #-} glpSolveVars ssimplexDefaults $ boxWorldLogicSolver constr obj
            return (not $ maxp > 0.0)
            where
                obj = objLess a b

boxWorldLogicSolver :: (Ord b, Ord a) => BWConstraints a b -> LinFunc (FreeProduct a b) Int -> LP (FreeProduct a b) Int
boxWorldLogicSolver constr obj = execLPM $ do
    setDirection Max
    setObjective obj
    mapM_ (\f -> equalTo f 1) $ normConstr constr
    mapM_ (\(f, g) -> equal f g) $ nonsigConstr constr
    mapM_ (\v -> varGeq v 0) $ vars constr
    mapM_ (\v -> setVarKind v ContVar) $ vars constr

-- boxWorldLogicSolver :: (Ord b, Ord a) => BWConstraints a b -> LinFunc (FreeProduct a b) Int -> LP (FreeProduct a b) Int
boxWorldLogicSolver' constr =  do
    setDirection Max
    -- setObjective obj
    mapM_ (\f -> equalTo f 1) $ normConstr constr
    mapM_ (\(f, g) -> equal f g) $ nonsigConstr constr
    mapM_ (\v -> varGeq v 0) $ vars constr
    mapM_ (\v -> setVarKind v ContVar) $ vars constr

addObj lp obj = do
        setObjective obj
        return

boxWorldConstraints :: (Ord a, Ord b) => FreeProduct a b -> [[a]] -> [[b]] -> BWConstraints a b 
boxWorldConstraints z as bs = BWConstraints { normConstr = normalization as bs
                                            , nonsigConstr = nonsignalling as bs
                                            , vars = boxWorldLPVars (concat as) (concat bs)
                                            , zeroElem = z }

data BWConstraints a b = BWConstraints { normConstr :: [LinFunc (FreeProduct a b) Int]
                                       , nonsigConstr :: [(LinFunc (FreeProduct a b) Int, LinFunc (FreeProduct a b) Int)]
                                       , vars :: [FreeProduct a b]
                                       , zeroElem :: FreeProduct a b }

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a:as) = (map (\b -> (a, b)) as) ++ pairs as
