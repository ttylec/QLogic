{-# LANGUAGE GADTs #-} 
module Data.QLogic.BoxWorld where

import Prelude hiding (null)
import Data.List hiding (union, null)
import Data.Maybe

import Data.Set ( Set, null
                , difference, union, intersection 
                , fromList, empty)
import qualified Data.Set as Set
-- import Data.Set ( Set, intersection, empty, singleton
--                 , isProperSubsetOf
--                 , toList, fromList)
import Data.Monoid (mappend)
import Control.Applicative hiding (empty)

import Data.Poset.ConcretePoset
import Data.QLogic

data Observable = Observable { name :: String
                             , domain :: [Int] } deriving (Eq, Ord, Show)

data AtomicQuestion = AtomicQuestion Observable Int |
                      Product AtomicQuestion AtomicQuestion deriving (Eq) 
                      
instance Show AtomicQuestion where
    show (AtomicQuestion obs val) = name obs ++ show val
    show (Product q1 q2) = show q1 ++ show q2

instance Ord AtomicQuestion where
    q@(AtomicQuestion oq vq) `compare` p@(AtomicQuestion op vp) = cname `mappend` cvals
        where
            cname = name oq `compare` name op
            cvals = vq `compare` vp
    (Product p1 q1) `compare` (Product p2 q2) = (p1 `compare` p2) `mappend` (q1 `compare` q2)
    (AtomicQuestion _ _) `compare` (Product _ _) = LT 
    (Product _ _) `compare` (AtomicQuestion _ _) = GT 

cprod :: AtomicQuestion -> AtomicQuestion -> AtomicQuestion
q@(AtomicQuestion _ _) `cprod` p@(AtomicQuestion _ _) 
    | q <= p = Product q p
    | otherwise = Product p q
p@(AtomicQuestion _ _) `cprod` q@(Product q0 qs)
    | p <= q0 = Product p q
    | otherwise = Product q0 $ p `cprod` qs
p@(Product _ _) `cprod` q@(AtomicQuestion _ _) = q `cprod` p
(Product p q) `cprod` r = p `cprod` (r `cprod` q)

atomicQuestionsOf :: Observable -> [AtomicQuestion]
atomicQuestionsOf obs = map (AtomicQuestion obs) $ domain obs

type OneSystem = AtomicQuestion
type TwoSystems = (AtomicQuestion, AtomicQuestion)

data PhaseSpace a = PhaseSpace (Set a) deriving (Show)

phaseSpace :: [Observable] -> PhaseSpace OneSystem
phaseSpace = PhaseSpace . fromList . phaseSpace'

phaseSpace2 :: [Observable] -> [Observable] -> PhaseSpace TwoSystems
phaseSpace2 o1 o2 = PhaseSpace $ fromList [(q, p) | q <- phaseSpace' o1, p <- phaseSpace' o2]

phaseSpace' []     = []
phaseSpace' (o:[]) = atomicQuestionsOf o
phaseSpace' (o:os) = cprod <$> atomicQuestionsOf o <*> phaseSpace' os

data TwoBoxQuestion = Composite AtomicQuestion AtomicQuestion | OPlus TwoBoxQuestion TwoBoxQuestion

instance Show TwoBoxQuestion where
    show (Composite p q) = "[" ++ show p ++ show q ++ "]"
    show (OPlus p q) = show p ++ "⊕" ++ show q

(<>) = Composite
(<+>) = OPlus

infixl 5 <>
infixl 4 <+>


boxQRepr2 :: PhaseSpace TwoSystems -> TwoBoxQuestion -> Set TwoSystems
boxQRepr2 (PhaseSpace phase) (Composite q1 q2) = Set.filter covered phase
    where
        covered (x, y) = (q1 `isBoxMemberOf` x) && (q2 `isBoxMemberOf` y)
boxQRepr2 phase (OPlus q1 q2) | null $ intersection a b = union a b
                              | otherwise = empty
                              where
                                  a = boxQRepr2 phase q1
                                  b = boxQRepr2 phase q2

atomFromRepr :: PhaseSpace TwoSystems -> [TwoBoxQuestion] -> Set TwoSystems -> Maybe TwoBoxQuestion
atomFromRepr phase atoms q = case filter ((q ==) . boxQRepr2 phase) atoms of
                               [] -> Nothing
                               (a:[]) -> Just a

fromRepr :: BWLogic -> PhaseSpace TwoSystems -> [TwoBoxQuestion] -> Set TwoSystems -> [TwoBoxQuestion]
fromRepr ql phase atoms q = map (toPlus . toAtoms) $ atomicDecomposition ql q 
    where
        toAtoms = map (fromJust . atomFromRepr phase atoms)
        toPlus = foldl1 (<+>) 

isBoxMemberOf :: AtomicQuestion -> AtomicQuestion -> Bool
q@(AtomicQuestion _ _) `isBoxMemberOf` p@(AtomicQuestion _ _) = p == q
q@(AtomicQuestion _ _) `isBoxMemberOf` (Product p1 p2) = (q `isBoxMemberOf` p1) 
                                                         || (q `isBoxMemberOf` p2)

boxWorldAtomicQs2 :: [Observable] -> [Observable] -> [TwoBoxQuestion]
boxWorldAtomicQs2 left right = [Composite qa qb | qa <- allQuestions left, qb <- allQuestions right]
    where
        allQuestions obs = concat $ map atomicQuestionsOf obs

concreteLogic :: (Ord a) => Set a -> [Set a] -> QLogic (ConcretePoset a) (Set a)
concreteLogic space els = fromPoset (ConcretePoset els) booleanOcmpl 
    where
        booleanOcmpl = difference space

boxWorldLogic2 :: [Observable] -> [Observable] -> (Set TwoSystems -> [TwoBoxQuestion], QLogic (ConcretePoset TwoSystems) (Set TwoSystems))
boxWorldLogic2 left right = (fromRepr ql phase atoms, ql)
    where
        ql = concreteLogic space els
        els = nub $ generateConcreteElems $ map (boxQRepr2 phase) atoms
        atoms = boxWorldAtomicQs2 left right 
        phase@(PhaseSpace space) = phaseSpace2 left right

generateConcreteElems :: (Ord a) => [Set a] -> [Set a]
generateConcreteElems [] = [empty]
generateConcreteElems (a:as) = generateConcreteElems as ++ 
                                    map (union a) (generateConcreteElems disjoint)
    where
        disjoint = filter (null . intersection a) as

type BWLElem = Set TwoSystems
type BWLogic = QLogic (ConcretePoset TwoSystems) (Set TwoSystems)

-- 
-- data TwoBoxState p = TwoBoxState (Map Question2 p)
-- 
-- toState :: (Real p) => TwoBoxState p -> State (QLogic (ConcretePoset TwoSystems) (Set TwoSystems) p
-- toState (TwoBoxState prstate) = \(

-- data Question where
--    Question :: Observable -> IntSet -> Question
--    Null :: Question
--    Trivial :: Question
--    Product :: Question -> Question -> Question  
--    
-- instance Show Question where
--     show (Question obs val) = name obs ++ "{" ++ intercalate "," (map show $ toList val) ++ "}"
--     show Null = "∅"
--     show Trivial = "1"
--     show (Product q1 q2) = show q1 ++ show q2
-- 
-- instance Eq Question where
--     Null == Null = True
--     Trivial == Trivial = True
--     (Question o1 v1) == (Question o2 v2) = o1 == o2 && v1 == v2
--     (Product q1 p1) == (Product q2 p2) = q1 == q2 && p1 == p2
-- 
-- instance Ord Question where
--     Null `compare`  _   = LT
--     _    `compare` Null = GT 
--     _    <= Trivial = True
--     (Question obs val) <= (Question obs val) = 

-- question :: Observable -> IntSet -> Question
-- question obs val
--     | val `isProperSubsetOf` domain obs = Question obs val
--     | val == domain obs = Trivial
--     | otherwise = Null

