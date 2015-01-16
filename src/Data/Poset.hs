{-# LANGUAGE GADTs, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Data.Poset (POrd, (.<=.), (.>=.), equivPOrd
                  , POrdStruct, lessIn, elementsOf
                  , Poset(Poset), fromFunc, fromPOrd, fromASRelation, quotientPoset
                  , fromFuncM
                  , isPoset
                  , packPoset, packPoset', unpackPoset, sparsePoset
                  , equalIn
                  , lubIn, glbIn 
                  , infIn, unsafeInfIn
                  , supIn, unsafeSupIn
                  , minimalIn, maximalIn
                  , propertyO2)
                  where

import Control.Monad
import Data.Maybe
import Data.Relation
import Data.Poset.Internals

import Data.QLogic.Utils

class POrdStruct a b | a -> b where
        elementsOf :: a -> [b]
        lessIn :: a -> b -> b -> Bool

-- | Class for types with partial order
class (Eq a, Ord a) => POrd a where
        (.<=.) :: a -> a -> Bool
        (.>=.) :: a -> a -> Bool
        equivPOrd :: a -> a -> Bool

        (.>=.) = flip (.<=.)
        equivPOrd a b = a .<=. b && b .<=. a

infix 4 .<=.
infix 4 .>=.

-- | Partially ordered set type. 
-- It has three flavours:
--
-- > Poset elements lessThanRelation
--
-- for the most general case, where lessThanRelation
-- is a function that defines partial order.
--
-- > SparsePoset elements sparseRelation
--
-- where relation is represented by SparseRelation.
-- Elements are still quite arbitrary.
-- Finally
--
-- > PackedPoset n relation
--
-- where elements are [0..n] and relation is 
-- represented by Relation, i.e. DIM2 repa array.
data Poset a where
        Poset :: (Eq a) => [a] -> Relation a -> Poset a 

instance (Show a) => Show (Poset a) where
        show poset = "No. of elements: " ++ (show $ length $ elementsOf poset) ++ 
                     "\nGreater than lists:\n" ++ (unlines $ gtlists)
            where
                gtlists = map (\a -> (show a) ++ "| " ++ (show $ geEqThan poset a)) $ elementsOf poset

instance POrdStruct (Poset a) a where
        elementsOf (Poset els _) = els
        lessIn (Poset _ rel) = inRelation rel

-- * Construction
-- |Constructs Poset from list of elements and relation given by function
fromFunc :: (Eq a) => [a] -> (a -> a -> Bool) -> Poset a
fromFunc els f = Poset els (Function f)

-- |Constructs Poset from monadic order function
fromFuncM els f = liftM (Poset els) $ relationFromFuncM els f

-- |Constructs Poset from the list of POrd data
fromPOrd :: (Eq a, POrd a) => [a] -> Poset a
fromPOrd els = Poset els (Function (.<=.))

-- |Converts Poset relation to list representation
sparsePoset :: (Ord a) => Poset a -> Poset a
sparsePoset (Poset els rel) = Poset els $ sparseRelation els rel

-- |Convert Poset to packed representation: elements are replaced
-- by sequence of integers and relation is coverted to array representation.
packPoset :: (Ord a) => Poset a -> Poset Int
packPoset = snd . packPoset'

packPoset' :: (Ord a) => Poset a -> (Packed a, Poset Int)
packPoset' (Poset els rel) = (packed, Poset [0..n-1] prel)
    where
        n = length els
        packed = packList els
        prel = packRelation packed rel

-- |Convert packed Poset representation to explicit one.
-- It is not safe: packed must have appropriate length.
unpackPoset :: (Ord a) => Packed a -> Poset Int -> Poset a
unpackPoset packed (Poset els rel) = Poset (packedElements packed) (Function isLess)
    where
        isLess = unpackFunc2 packed (inRelation rel)

-- |Takes a set with anti-symmetric, reflexive relation 
-- and creates preposet by taking transitive closure of the relation.
fromASRelation :: (Ord a) => Poset a -> Poset a
fromASRelation set@(Poset els rel@(Function _)) = unpackPoset packed preposet
        where
            (packed, (Poset pels pr)) = packPoset' set
            preposet = Poset pels $ transitiveClosure pels pr
fromASRelation (Poset els rel) = Poset els $ transitiveClosure els rel


-- |Preorder is a partial order relation that is anti-symmetric
-- and transitive but not reflexive. Having a set with a preorder
-- we can construct partialy ordered set by taking the quotient:
quotientPoset :: (Ord a) => Poset a -> Poset (Equiv a)
quotientPoset preposet = Poset els equivLess
    where
        els = quotientBy equiv $ elementsOf preposet
        equiv = \a b -> a ≤ b && b ≤ a
        (≤) = lessIn preposet
        equivLess = Function $ liftFunc2 $ lessIn preposet

isPoset :: Poset a -> Bool
isPoset (Poset els rel) = isReflexive els rel && isAntiSymmetric els rel && isTransitive els rel

-- * Basic queries
-- |Check if two elements are equal in relation
equalIn :: (POrdStruct p a) => p -> a -> a -> Bool
equalIn poset a b = lessIn poset a b && lessIn poset b a

-- | Set of least upper bounds of two elements,
-- i.e. minimal elements in the set of elements
-- that are greater than both.
lubIn :: (POrdStruct p a) => p -> a -> a -> [a] 
lubIn poset a b = minimalIn poset $ filter (a ≤) $ filter (b ≤) els
    where
        els = elementsOf poset
        (≤) = lessIn poset

-- | Set of greatest lower bounds of two elements
-- i.e. maximal elements in the set of elements
-- that are less than both.
glbIn :: (POrdStruct p a) => p -> a -> a -> [a] 
glbIn poset a b = maximalIn poset $ filter (≤ a) $ filter (≤ b) els
    where
        els = elementsOf poset
        (≤) = lessIn poset

-- |Lowest upper bound of pair of elements (join).
supIn :: (POrdStruct p a) => p -> a -> a -> Maybe a
supIn poset a b  
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
        lub = lubIn poset a b

-- |Unsafe lowest upper bound: assumes that it exists.
unsafeSupIn :: (POrdStruct p a) => p -> a -> a -> a
unsafeSupIn poset a b = fromJust $ supIn poset a b

-- |Lowest upper bound of pair of elements (join).
infIn :: (POrdStruct p a) => p -> a -> a -> Maybe a
infIn poset a b  
    | length glb == 1 = Just $ head glb
    | otherwise = Nothing
    where
        glb = glbIn poset a b

-- |Unsafe lowest upper bound: assumes that it exists.
unsafeInfIn :: (POrdStruct p a) => p -> a -> a -> a
unsafeInfIn poset a b = fromJust $ infIn poset a b

-- |List of elements that are greater or equal than given one
geEqThan :: (POrdStruct p a) => p -> a -> [a]
geEqThan poset a = filter (lessIn poset a) $ elementsOf poset

-- |Returns subset of minimal elements in given set,
-- i.e. elements that are not greater than any of the
-- other elements of the set.
minimalIn :: (POrdStruct p a) => p -> [a] -> [a]
minimalIn _ [] = []
minimalIn _ [a] = [a]
minimalIn poset (a:as)
    | any (≤ a) as = minimalIn poset as
    | otherwise = a:(minimalIn poset $ filter (not . (a ≤)) as)
    where
        (≤) = lessIn poset

-- |Returns subset of maximal elements in given set,
-- i.e. element that are not less than any of the
-- other lements of the set.
maximalIn :: (POrdStruct p a) => p -> [a] -> [a]
maximalIn _ [] = []
maximalIn _ [a] = [a]
maximalIn poset (a:as)
    | any (a ≤) as = maximalIn poset as
    | otherwise = a:(maximalIn poset $ filter (not . (≤ a)) as)
    where
        (≤) = lessIn poset


-- * Testing various properties
--
propertyO2 :: (POrdStruct p a) => p -> (a -> a -> Maybe a) -> Bool
propertyO2 poset op = and [a ≤ b `iff` any (\a' -> b `equal` (a `op` a')) els | a <- els, b <- els]
    where
        els = elementsOf poset
        (≤) = lessIn poset
        b `equal` ma = case ma of
                           Nothing -> False
                           Just a -> equalIn poset b a

iff :: Bool -> Bool -> Bool
iff False False = True
iff False True = False
iff True False = False
iff True True = True
