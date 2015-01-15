{-# LANGUAGE GADTs, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Data.QLogic.BoxProduct ((<>), (<+>)
                              , boxQuestions, boxAtomicQuestions
                              , boxProduct, boxAtomicProduct
                              , boxPlus
                              , boxProduct'  -- TO BE REMOVED
                              , FreeProduct(FreeProd, FreePlus)
                              ) where

import Data.List
import Data.Maybe
import Data.Monoid hiding ((<>))

import Data.Poset
import Data.Poset.Internals
import Data.QLogic

-- = FreeProduct
-- | Free product of quantum logics. 
-- A formal object that is used in construction of box product.
-- Represents questions in product system,
-- while elements of box product represent propositions.
data FreeProduct a b = FreeProd a b | FreePlus (FreeProduct a b) (FreeProduct a b)

instance (Show a, Show b) => Show (FreeProduct a b) where
        show (FreeProd a b) = (show a) ++ (show b)
        show (FreePlus a p) = (show a) ++ "⊕" ++ (show p)

-- | Natural total order on FreeProduct of ordered data.
-- It is lexicographic order: firstly the first components are compared,
-- if equal then the second.
instance (Ord a, Ord b) => Ord (FreeProduct a b) where
        (FreeProd a1 a2) `compare` (FreeProd b1 b2) = (a1 `compare` b1) `mappend` (a2 `compare` b2)
        (FreeProd _ _) `compare` (FreePlus _ _) = LT
        (FreePlus _ _) `compare` (FreeProd _ _) = GT
        (FreePlus a as) `compare` (FreePlus b bs) = (a `compare` b) `mappend` (as `compare` bs)

-- | Formal equality.
instance (Eq a, Eq b) => Eq (FreeProduct a b) where
        (FreeProd a1 a2) == (FreeProd b1 b2) = a1 == b1 && a2 == b2
        (FreePlus a as) == (FreePlus b bs) = a == b && as == bs
        _ == _ = False

-- | Partial order on FreeProduct
instance (POrd a, POrd b) => POrd (FreeProduct a b) where
        (FreeProd a1 a2) .<=. (FreeProd b1 b2) = a1 .<=. b1 && a2 .<=. b2
        a@(FreeProd _ _) .<=. (FreePlus b bs) = a .<=. b || a .<=. bs
        (FreePlus a as) .<=. b = a .<=. b && as .<=. b

-- | Constructs the FreeProduct of two elements.
-- Syntatic sugar for FreeProd.
(<>) :: (Ord a, Ord b) => a -> b -> FreeProduct a b
(<>) a b = FreeProd a b
infixl 5 <>

-- | Construct FreePlus of two FreeProducts.
-- FreePlus is commutative by definition,
-- thus to make formal equality (Eq instance) work, 
-- we assume that the order of elements (w.r.t to total lexical order)
-- in the sum is asceding.
(<+>) :: (Ord a, Ord b) => FreeProduct a b -> FreeProduct a b -> FreeProduct a b
(<+>) a@(FreeProd _ _) b@(FreeProd _ _)
    | a <= b = FreePlus a b
    | otherwise = FreePlus b a
(<+>) a@(FreeProd _ _) p@(FreePlus b bs)
    | a <= b = FreePlus a p
    | otherwise = FreePlus b (a <+> bs)
(<+>) a@(FreePlus _ _) b@(FreeProd _ _) = b <+> a
(<+>) a@(FreePlus a1 a2) b@(FreePlus _ _) = a1 <+> (a2 <+> b)
infixl 4 <+>

-- = Questions of composite system

-- | Disjoint relation for questions
-- We start the construction by defining which questions are disjoint.
-- TODO: fill reference to paper
freeDisj :: (Ord a, Ord b) => (QLogic a, QLogic b) -> FreeProduct a b -> FreeProduct a b -> Bool
freeDisj (qla, qlb) (FreeProd a1 a2) (FreeProd b1 b2) = (disjointIn qla a1 b1) || (disjointIn qlb a2 b2)
freeDisj ql a@(FreeProd _ _) (FreePlus b bs) = (freeDisj ql a b) && (freeDisj ql a bs)
freeDisj ql (FreePlus a as) b = (freeDisj ql a b) && (freeDisj ql as b)

-- | Returns the list of all questions in box product of two logics.
boxQuestions :: (Ord a, Ord b) => QLogic a -> QLogic b -> [FreeProduct a b]
boxQuestions qla qlb = zerozero:boxQuestions' ortho pairs
        where
            zerozero = zeroOf qla <> zeroOf qlb
            ortho = freeDisj (qla, qlb)
            pairs = [a <> b | a <- elementsOf qla, b <- elementsOf qlb, a /= zeroOf qla, b /= zeroOf qlb]
            
-- | Returns the list of questions formed by atoms in box product of
-- two logics. 
-- TODO: it's not clear if this gives equivalent structure
-- to the one obtained with boxQuestions. 
boxAtomicQuestions :: (Ord a, Ord b) => QLogic a -> QLogic b -> [FreeProduct a b]
boxAtomicQuestions qla qlb = zero:boxQuestions' ortho boxAtoms
    where
        boxAtoms = [a <> b | a <- atomsOf qla, b <- atomsOf qlb]
        ortho = freeDisj (qla, qlb)
        zero = zeroOf qla <> zeroOf qlb

boxQuestions' :: (Ord a, Ord b) => (FreeProduct a b -> FreeProduct a b -> Bool) -> 
    [FreeProduct a b] -> [FreeProduct a b]
boxQuestions' _ [] = []
boxQuestions' ortho (p:ps) = allBoxSums ortho orthoPairs p ++ boxQuestions' ortho ps
    where
        orthoPairs = filter (ortho p) ps
        
allBoxSums :: (Ord a, Ord b) => (FreeProduct a b -> FreeProduct a b -> Bool) -> 
    [FreeProduct a b] -> FreeProduct a b -> [FreeProduct a b]
allBoxSums _ [] a = [a]
allBoxSums ortho (p:ps) a = (allBoxSums ortho ps a) ++ (allBoxSums ortho orthoPairs $ p <+> a)
    where
        orthoPairs = filter (ortho p) ps

-- | See: [preprint]
rightOf :: (Ord a, Ord b) => (QLogic a, QLogic b) -> a -> FreeProduct a b -> b
rightOf ql@(qla, qlb) c a
    | c /= zeroOf qla = rightOf' ql (zeroOf qlb) c a
    | otherwise = oneOf qlb 

rightOf' (qla, qlb) accum c (FreeProd a b)
    | a ≥ c = accum \/ b
    | otherwise = accum
    where
        (≥) = flip $ lessIn qla
        (\/) = unsafeSupIn qlb
rightOf' qlb accum c (FreePlus a as) = rightOf' qlb (rightOf' qlb accum c a) c as

-- | See: [preprint]
leftOf :: (Ord a, Ord b) => (QLogic a, QLogic b) -> b -> FreeProduct a b -> a
leftOf ql@(qla, qlb) d b
    | d /= zeroOf qlb = leftOf' ql (zeroOf qla) d b
    | otherwise = oneOf qla

leftOf' (qla, qlb) accum d (FreeProd a b)
    | b ≥ d = accum \/ a
    | otherwise = accum
    where
        (≥) = flip $ lessIn qlb
        (\/) = unsafeSupIn qla
leftOf' qla accum d (FreePlus a as) = leftOf' qla (leftOf' qla accum d a) d as

-- | Pre-order relation on box product questions.
boxPrec :: (Ord a, Ord b) => (QLogic a, QLogic b) -> FreeProduct a b -> FreeProduct a b -> Bool
boxPrec (qla, qlb) (FreeProd a b) (FreeProd c d) = a `leftLess` c && b `rightLess` d
    where
        leftLess = lessIn qla
        rightLess = lessIn qlb
boxPrec ql@(qla, qlb) ab@(FreeProd a b) p = a `leftLess` (leftOf ql b p) || b `rightLess` (rightOf ql a p)
    where
        leftLess = lessIn qla
        rightLess = lessIn qlb
boxPrec ql (FreePlus a as) p = (boxPrec ql a p) && (boxPrec ql as p)

-- = Logic of composite system
-- | BoxProduct is the equivalence class of FreeProduct's,
-- because a proposition is an equivalence class of questions.
type BoxProduct a b = Equiv (FreeProduct a b)

boxProduct :: (Ord a, Ord b) => QLogic a -> QLogic b -> Poset (BoxProduct a b)
boxProduct qla qlb = quotientPoset $ boxProduct' qla qlb $ boxQuestions qla qlb

boxAtomicProduct :: (Ord a, Ord b) => QLogic a -> QLogic b -> Poset (BoxProduct a b)
boxAtomicProduct qla qlb = quotientPoset $ boxProduct' qla qlb $ boxAtomicQuestions qla qlb

boxProduct' :: (Ord a, Ord b) => QLogic a -> QLogic b -> [FreeProduct a b] -> Poset (FreeProduct a b)
boxProduct' qla qlb questions = fromASRelation $ fromFunc questions $ boxPrec (qla, qlb)

-- These functions need separate proof of correctness. 
-- It looks quite obvious, that we can define them in that way
-- but that's sth different than what is in the paper.
-- Here, after createing Poset (BoxProduct a b) we loose reference
-- to QLogic a and QLogic b so we have problem with lifting freeDisj.
boxDisjoint :: (Ord a, Ord b) => QLogic (BoxProduct a b) -> BoxProduct a b -> BoxProduct a b -> Bool
boxDisjoint ql a b = case boxPlus ql a b of
                         Nothing -> False
                         Just _ -> True 

boxPlus :: (Ord a, Ord b) => QLogic (BoxProduct a b) -> BoxProduct a b -> BoxProduct a b -> Maybe (BoxProduct a b)
boxPlus ql a b 
    | a == zeroOf ql = Just b
    | b == zeroOf ql = Just a
    | otherwise = equivLookup (elementsOf ql) $ (equivRepr a) <+> (equivRepr b)

freePlus :: (Ord a, Ord b) => (QLogic a, QLogic b) -> Poset (FreeProduct a b)
    -> FreeProduct a b -> FreeProduct a b -> Maybe (FreeProduct a b)
freePlus qls@(qla, qlb) poset a b 
    | a == zero = Just b
    | b == zero = Just a 
    | freeDisj qls a b == True = Just $ a <+> b
    | otherwise = Nothing
    where
        zero = zeroOf qla <> zeroOf qlb
