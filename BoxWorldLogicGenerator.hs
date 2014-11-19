{-# LANGUAGE GADTs, BangPatterns #-}
module BoxWorldLogicGenerator where

import Data.Monoid

import Control.Monad.LPMonad
import Data.LinearProgram
import Data.LinearProgram.GLPK

data Box2 = X0 | X1 | Y0 | Y1 deriving (Eq, Ord, Show)

data FreeProduct a b = (Ord a, Ord b) => FreeProd a b | FreePlus (FreeProduct a b) (FreeProduct a b)

(/\) :: (Ord a, Ord b) => a -> b -> FreeProduct a b
(/\) = FreeProd

(<+>) :: FreeProduct a b -> FreeProduct a b -> FreeProduct a b
(<+>) a@(FreeProd _ _) b@(FreeProd _ _)
    | a <= b = FreePlus a b
    | otherwise = FreePlus b a
(<+>) a@(FreeProd _ _) b@(FreePlus b0 bs) 
    | a <= b0 = FreePlus a b
    | otherwise = FreePlus a $ b <+> bs
(<+>) a@(FreePlus _ _) b@(FreeProd _ _) = FreePlus b a
(<+>) a@(FreePlus a1 a2) b@(FreePlus _ _) = a1 <+> (a2 <+> b)

freeToList :: FreeProduct a b -> [FreeProduct a b]
freeToList a@(FreeProd _ _) = [a]
freeToList (FreePlus a b) = a:(freeToList b)

instance (Eq a, Eq b) => Eq (FreeProduct a b) where
        (FreeProd a1 b1) == (FreeProd a2 b2) = a1 == a2 && b1 == b2
        (FreePlus a as) == (FreePlus b bs) = a == b && as == bs
        _ == _ = False

instance (Ord a, Ord b) => Ord (FreeProduct a b) where
        compare (FreeProd a1 b1) (FreeProd a2 b2) = compare a1 a2 <> compare b1 b2
        compare (FreeProd _ _) (FreePlus _ _) = LT
        compare (FreePlus a as) (FreePlus b bs) = compare a b <> compare as bs
        compare _ _ = GT

instance (Show a, Show b) => Show (FreeProduct a b) where
        show (FreeProd a b) = (show a) ++ (show b)
        show (FreePlus a p) = (show a) ++ "⊕" ++ (show p)

boxQ = [[X0, X1], [Y0, Y1]]

boxWorldLPVars :: (Ord a, Ord b) => [a] -> [b] -> [FreeProduct a b]
boxWorldLPVars as bs = [a /\ b | a <- as, b <- bs]

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
nsLeft bbs a = pairs [varSum [a /\ b | b <- bs] | bs <- bbs]

nsRight :: (Ord a, Ord b) => [[a]] -> b ->
    [(LinFunc (FreeProduct a b) Int, LinFunc (FreeProduct a b) Int)]
nsRight aas b = pairs [varSum [a /\ b | a <- as] | as <- aas]

isQuestionObj :: (Ord a, Ord b) => FreeProduct a b -> LinFunc (FreeProduct a b) Int
isQuestionObj a = varSum $ freeToList a 

-- isQuestion lpsolver a 
--     | maxp > 1.0 = False
--     | otherwise = True
--     where
--         (_, Just (maxp, _)) = glpSolveVars simplexDefaults $ boxWorldLogicSolver lpsolver obj
--         obj = isQuestionObj a

isQuestion lpsolver a = glpSolveVars simplexDefaults $ boxWorldLogicSolver lpsolver obj
    where
        obj = isQuestionObj a


boxWorldLogicSolver lpsolver obj = execLPM $ do
    lpsolver
    setObjective obj

boxWorldLogicGLPKSolver as bs = do
    let vars = boxWorldLPVars (concat as) (concat bs)
    setDirection Max
    -- setObjective obj
    mapM_ (\f -> equalTo f 1) $ normalization as bs
    mapM_ (\(f, g) -> equal f g) $ nonsignalling as bs
    mapM_ (\v -> varGeq v 0) vars 
    mapM_ (\v -> setVarKind v ContVar) vars

-- boxWorldLogicSolver as bs obj = execLPM $ do
--     let vars = boxWorldLPVars (concat as) (concat bs)
--     setDirection Max
--     setObjective obj
--     mapM_ (\f -> equalTo f 1) $ normalization as bs
--     mapM_ (\(f, g) -> equal f g) $ nonsignalling as bs
--     mapM_ (\v -> varGeq v 0) vars 
--     mapM_ (\v -> setVarKind v ContVar) vars

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a:as) = (map (\b -> (a, b)) as) ++ pairs as
