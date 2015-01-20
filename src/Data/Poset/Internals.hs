{-# LANGUAGE GADTs, BangPatterns #-}

module Data.Poset.Internals (Equiv(Equiv), liftFunc, liftFunc2, quotientBy
                            , equivRepr, equivLookup
                            , Packed, packedElements, packList
                            , packedLength
                            , toKey, fromKey
                            , packFunc, unpackFunc
                            , packFunc2, unpackFunc2) 
                            where

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

data Equiv a where
       Equiv :: (Ord a) => [a] -> Equiv a

instance (Show a) => Show (Equiv a) where
        show a = "[[" ++ (show $ equivRepr a) ++ "]]"

instance (Eq a) => Eq (Equiv a) where
        (Equiv a) == (Equiv b) = any (`elem` b) a

instance (Ord a) => Ord (Equiv a) where
        a `compare` b = equivRepr a `compare` equivRepr b

liftFunc :: [Equiv a] -> (a -> a) -> Equiv a -> Equiv a
liftFunc els f = fromJust . (equivLookup els) . f . equivRepr

liftFunc2 :: (a -> a -> Bool) -> Equiv a -> Equiv a -> Bool
liftFunc2 rel a b = equivRepr a `rel` equivRepr b

equivRepr :: Equiv a -> a
equivRepr (Equiv e) = minimum  e

equivLookup :: [Equiv a] -> a -> Maybe (Equiv a)
equivLookup [] _ = Nothing
equivLookup (e@(Equiv ec):es) a
    | a `elem` ec = Just e
    | otherwise = equivLookup es a

quotientBy :: (Ord a) => (a -> a -> Bool) -> [a] -> [Equiv a]
quotientBy eq = quotientBy' eq []

quotientBy' :: Ord a => (a -> a -> Bool) -> [Equiv a] -> [a] -> [Equiv a]
quotientBy' _ accum [] = accum
quotientBy' eq accum as@(a:_) = quotientBy' eq ((Equiv equal):accum) rest
    where
        (equal, rest) = partition (eq a) as

-- |Dictonary for "packing" abstract structures,
-- for optimized operations using repa arrays.
data Packed a = Packed (Map.Map a Int) (V.Vector a)

instance Show a => Show (Packed a) where
        show (Packed _ a) = "Packed: " ++ (unwords $ V.toList $ V.imap (\i a -> (show i) ++ ": " ++ show a) a)

packedLength :: Packed a -> Int
packedLength (Packed _ a) = V.length a

packedElements :: Packed a -> [a]
packedElements (Packed _ a) = V.toList a

-- |Convert list of elements to Packed
packList :: (Ord a) => [a] -> Packed a
packList els = Packed dict vals
    where
        vals = V.fromList els
        dict = Map.fromList $ zip els [0..]

-- |Get key of element in Packed
toKey :: (Ord a) => Packed a -> a -> Int
toKey (Packed dict _) a = case Map.lookup a dict of
                                  Nothing -> error "No such element"
                                  Just i -> i

-- |Get element from key in Packed
fromKey:: (Ord a) => Packed a -> Int -> a
fromKey (Packed _ dict) i = dict V.! i

-- |Lifts a function to Packed keys
packFunc :: (Ord a) => Packed a -> (a -> a) -> Int -> Int
packFunc idx f = (toKey idx) . f . (fromKey idx)

-- |Lifts a binary function to Packed keys
unpackFunc :: (Ord a) => Packed a -> (Int -> Int) -> a -> a
unpackFunc idx f = (fromKey idx) . f . (toKey idx)

-- |Lifts a binary function to Packed keys
packFunc2 :: (Ord a) => Packed a -> (a -> a -> b) -> Int -> Int -> b
packFunc2 idx f = \a b -> f (fromKey idx a) (fromKey idx b)

-- |Lifts a binary function to Packed keys
unpackFunc2 :: (Ord a) => Packed a -> (Int -> Int -> b) -> a -> a -> b
unpackFunc2 idx f = \i j -> f (toKey idx i) (toKey idx j)
