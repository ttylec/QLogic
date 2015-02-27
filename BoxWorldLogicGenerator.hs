{-# LANGUAGE GADTs, ExistentialQuantification #-}
module Main where

import Data.Monoid

import Control.Monad
import Control.Monad.LPMonad
import Data.LinearProgram
import Data.LinearProgram.GLPK

import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Functor.Identity (Identity)

ssimplexDefaults = SimplexOpts MsgOff 10000 True

-- I don't know why... Haskell type system said so, I add it to
-- have explicit types everywhere
data BWSolver a b = forall a1. BWSolver (StateT (LP (FreeProduct a b) Int) Identity a1)

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

infixr 8 <+>
infix  9 /\

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
isQuestionObj a@(FreeProd _ _) = var a
isQuestionObj (FreePlus a as) = (var a) ^+^ isQuestionObj as

boxWorldLessObj :: (Ord a, Ord b) => FreeProduct a b -> FreeProduct a b -> LinFunc (FreeProduct a b) Int
boxWorldLessObj a b = (isQuestionObj a) ^-^ (isQuestionObj b)

isQuestion :: (Ord a, Ord b) => BWSolver a b -> FreeProduct a b -> IO Bool
isQuestion (BWSolver lpsolver) a = do
        (_, Just (maxp, _)) <- glpSolveVars ssimplexDefaults $ boxWorldLogicSolver lpsolver obj
        -- putStrLn $ show $ obj
        return (maxp <= 1.0)
        where
            obj = isQuestionObj a

boxLess :: (Ord a, Ord b) => BWSolver a b -> 
    FreeProduct a b -> FreeProduct a b -> IO Bool
boxLess (BWSolver lpsolver) a b =  do
        (_, Just (maxp, _)) <- {-# SCC solver #-} glpSolveVars ssimplexDefaults $ boxWorldLogicSolver lpsolver obj
        return (not $ maxp > 0.0)
        where
            obj = boxWorldLessObj a b

boxEqual :: (Ord a, Ord b) => BWSolver a b -> 
    FreeProduct a b -> FreeProduct a b -> IO Bool
boxEqual lp a b = liftM2 (&&) (boxLess lp a b) (boxLess lp b a)

boxNotEqual :: (Ord a, Ord b) => BWSolver a b -> 
    FreeProduct a b -> FreeProduct a b -> IO Bool
boxNotEqual lp a b = liftM not $ boxEqual lp a b

extendBWList :: (Ord a, Ord b) => (BWSolver a b) ->
    [FreeProduct a b] -> [FreeProduct a b] -> IO [FreeProduct a b]
extendBWList lpsolver qs atoms = do
        qs <- filterM (isQuestion lpsolver) [q <+> a | q <- qs, a <- atoms]  
        nubByM (boxEqual lpsolver) qs

boxWorldAtoms :: (Ord a, Ord b) => [[a]] -> [[b]] -> [FreeProduct a b]
boxWorldAtoms as bs = [a/\b | a <- (concat $ as), b <- (concat $bs)]

boxWorldLogicSolver lpsolver obj = execLPM $ do
    lpsolver
    setObjective obj

boxWorldLogicGLPKSolver as bs = execLPT $ do
    let vars = boxWorldLPVars (concat as) (concat bs)
    setDirection Max
    mapM_ (\f -> equalTo f 1) $ normalization as bs
    mapM_ (\(f, g) -> equal f g) $ nonsignalling as bs
    mapM_ (\v -> varGeq v 0) vars 
    mapM_ (\v -> setVarKind v ContVar) vars

boxWorldLogicSolver' as bs obj = execLPM $ do
    let vars = boxWorldLPVars (concat as) (concat bs)
    setDirection Max
    setObjective obj
    mapM_ (\f -> equalTo f 1) $ normalization as bs
    mapM_ (\(f, g) -> equal f g) $ nonsignalling as bs
    mapM_ (\v -> varGeq v 0) vars 
    mapM_ (\v -> setVarKind v ContVar) vars

isQuestion' as bs a = do
        (_, Just (maxp, _)) <- glpSolveVars ssimplexDefaults $ boxWorldLogicSolver' as bs obj
        putStrLn $ show $ obj
        return (maxp <= 1.0)
        where
            obj = isQuestionObj a


-- nubByM :: (Monad m) => (a -> a -> m Bool) -> m [a] -> m [a]
-- nubByM eq mas = do
--         (a:as) <- mas
--         noteq <- filterM ((liftM not) . (eq a)) as
--         
--         liftM2 (:) (return a) (nubByM eq noteq)

nubByM :: (Monad m) => (a -> a -> m Bool) -> [a] -> m [a]
nubByM eq [] = return []
nubByM eq (a:as) = do
        noteq <- filterM ((liftM not) . (eq a)) as
        liftM (a:) (nubByM eq noteq)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a:as) = (map (\b -> (a, b)) as) ++ pairs as


--
-- Defs for testing
--
boxQ = [[X0, X1], [Y0, Y1]]
lps = BWSolver $ boxWorldLogicGLPKSolver boxQ boxQ

main = do
  qa <- isQuestion' boxQ boxQ $ X0 /\ X1
  putStrLn $ show $ qa
-- boxQ = [[X0, X1], [Y0, Y1]]
-- atoms = boxWorldAtoms boxQ boxQ 
-- lps = BWSolver $ boxWorldLogicGLPKSolver boxQ boxQ
