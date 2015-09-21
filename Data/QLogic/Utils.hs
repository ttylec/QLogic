{-# LANGUAGE GADTs, BangPatterns #-}
module Data.QLogic.Utils (implies, iff, mutuallyBy, subsets, subsetsBy, tuples --, subsetsBy', subsets'
                         , Equiv(Equiv), liftFunc, liftFunc2, quotientBy
                         , equivRepr, equivLookup
                         , Packed(Packed), packedElements, packList
                         , packedLength
                         , toKey, fromKey
                         , packFunc, unpackFunc
                         , packFunc2, unpackFunc2) where

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

-- import Control.Monad
-- |
-- = Auxiliary data structures and functions
-- 

-- |
-- == Logical operations
-- We often try to check if some property is satisfied,
-- what basically means that we need to check if some
-- logical sentence is valid. 
--
-- These functions allows to write such formulas
-- in more mathematical manner, utilizing commonly
-- used phrases like 'implies' and 'iff' (if and only if)

-- |Logical implication
implies :: Bool -> Bool -> Bool
implies False False = True
implies False True = True
implies True False = False
implies True True = True

-- |Logical if and only if
iff :: Bool -> Bool -> Bool
iff False False = True
iff False True = False
iff True False = False
iff True True = True

-- |
-- == Combinatorial operations

-- |Utitlity function to perform "mutuall" tests.
mutuallyBy :: (a -> a -> Bool) -> [a] -> Bool
mutuallyBy _ [] = True
mutuallyBy f (a:as) = (all (f a) as) && mutuallyBy f as

-- |List of tuples of given length with values from vals.
tuples :: [a] -> Int -> [[a]]
tuples vals n = mapM (const vals) [0..n-1]

-- | Subsets of a given list (uniqueness of elements is not checked)
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- choose :: [a] -> Int -> [[a]]
-- choose _ 0 = [[]]
-- choose [] _ = []
-- choose (x:xs) k = (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

-- subsets' :: [a] -> [[a]]
-- subsets' xs = concat $ map (xs `choose`) [0..length xs] 
--
-- This is clever implementation, but I don't yet understand how it works,
-- but maybe this could lead to more efficient subsetsBy implementation.
-- subsets' = filterM (const [True, False])
-- | Subsets of a given list, such that elements 
-- pairwisely satisfy given binary and transitive relation.
-- 
-- Properties:
-- > all (mutuallyBy pred) (subsetsBy pred ls) == True
-- 
-- It's a manual rewrite of:
-- > subsetsBy pred ls@(x:xs) = filter (mutuallyBy pred) $ subsets ls
subsetsBy :: (a -> a -> Bool) -> [a] -> [[a]]
subsetsBy _ [] = [[]]
subsetsBy pred (x:xs) = subsetsBy pred xs ++ map (x:) (subsetsBy pred $ filter (pred x) xs)

-- subsetsBy' :: (a -> a -> Bool) -> [a] -> [[a]]
-- subsetsBy' pred xs = filter (mutuallyBy pred) $ subsets' xs

-- | Data type reprenting equivalence class as sets
data Equiv a where
       Equiv :: (Ord a) => [a] -> Equiv a

instance (Show a) => Show (Equiv a) where
        show a = "[[" ++ (show $ equivRepr a) ++ "]]"

instance (Eq a) => Eq (Equiv a) where
        (Equiv a) == (Equiv b) = any (`elem` b) a

instance (Ord a) => Ord (Equiv a) where
        a `compare` b = equivRepr a `compare` equivRepr b

-- | Lift map on elements to map on equivalence classes.
-- Function should respect equivalence classes.
liftFunc :: [Equiv a] -> (a -> a) -> Equiv a -> Equiv a
liftFunc els f = fromJust . (equivLookup els) . f . equivRepr

-- | Lift binary function on elements to equivalence classes.
-- Function should respect equivalence classes.
liftFunc2 :: (a -> a -> b) -> Equiv a -> Equiv a -> b
liftFunc2 f a b = f (equivRepr a) (equivRepr b)

-- | Lift relation to equivalence classes in transitive way,
-- i.e. equivalence classes are in relation iff there exists a 
-- pair of representatives that are in relation. 
liftRelT :: (a -> a -> Bool) -> Equiv a -> Equiv a -> Bool
liftRelT rel (Equiv as) (Equiv bs) = or [a `rel` b | a <- as, b <- bs]

-- | Return representant of equivalence class.
equivRepr :: Equiv a -> a
equivRepr (Equiv e) = minimum  e

-- | Find equivalence class to which element belongs.
equivLookup :: [Equiv a] -> a -> Maybe (Equiv a)
equivLookup [] _ = Nothing
equivLookup (e@(Equiv ec):es) a
    | a `elem` ec = Just e
    | otherwise = equivLookup es a

-- | Compute quotient of the given list by equivalence
-- relation. 
quotientBy :: (Ord a) => (a -> a -> Bool) -> [a] -> [Equiv a]
quotientBy eq = quotientBy' eq []

quotientBy' :: Ord a => (a -> a -> Bool) -> [Equiv a] -> [a] -> [Equiv a]
quotientBy' _ accum [] = accum
quotientBy' eq !accum as@(a:_) = quotientBy' eq ((Equiv equal):accum) rest
    where
        (equal, rest) = partition (eq a) as

-- |
-- == Structure for easy and efficient two-way lookup

-- |Dictonary for packing abstract structures,
-- for optimized operations using e.g. repa arrays.
data Packed a where
   Packed :: (Ord a) => Map.Map a Int -> V.Vector a -> Packed a

instance Show a => Show (Packed a) where
        show (Packed _ a) = "Packed: " ++ (unwords $ V.toList $ V.imap (\i a -> (show i) ++ ": " ++ show a) a)

-- | Return number of elements in Packed structure.
packedLength :: Packed a -> Int
packedLength (Packed _ a) = V.length a

-- | Return list of Packed elements.
packedElements :: Packed a -> [a]
packedElements (Packed _ a) = V.toList a

-- | Convert list of elements to Packed
packList :: (Ord a) => [a] -> Packed a
packList els = Packed dict vals
    where
        vals = V.fromList els
        dict = Map.fromList $ zip els [0..]

-- | Get key of an element in Packed
toKey :: (Ord a) => Packed a -> a -> Int
toKey (Packed dict _) a = case Map.lookup a dict of
                                  Nothing -> error "No such element"
                                  Just i -> i

-- | Get element corresponding to given key.
fromKey:: (Ord a) => Packed a -> Int -> a
fromKey (Packed _ dict) i = dict V.! i

-- | Converts function on keys to function on elements.
packFunc :: (Ord a) => Packed a -> (a -> a) -> Int -> Int
packFunc idx f = (toKey idx) . f . (fromKey idx)

-- | Convert function on keys to function on elements.
unpackFunc :: (Ord a) => Packed a -> (Int -> Int) -> a -> a
unpackFunc idx f = (fromKey idx) . f . (toKey idx)

-- | Converts binary function on keys to function on elements.
packFunc2 :: (Ord a) => Packed a -> (a -> a -> b) -> Int -> Int -> b
packFunc2 idx f = \a b -> f (fromKey idx a) (fromKey idx b)

-- | Convert function on keys to function on elements.
unpackFunc2 :: (Ord a) => Packed a -> (Int -> Int -> b) -> a -> a -> b
unpackFunc2 idx f = \i j -> f (toKey idx i) (toKey idx j)
