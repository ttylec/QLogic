{-# LANGUAGE GADTs, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module QLogic.Poset (POrd, (.<=.), (.>=.), equiv
                    , POrdStruct, lessIn, elementsOf
                    , isPoset
                    , equalIn
                    , lubIn, glbIn
                    , infIn, unsafeInfIn
                    , supIn, unsafeSupIn
                    , geEqThan, leEqThan
                    , minimalIn, maximalIn
                    , propertyO2)
       where

import Data.Maybe

import QLogic.Utils
import QLogic.Relation

-- * Partially ordered sets

-- | Partially ordered sets (as an algebraic structures).
-- Class has two parameters: @p@ is an algebraic structure
-- which elements are of type @a@ (functional dependency).
-- So the partial order is related more to type of the structure
-- than the type of elements.  See 'POrd' for types with partial ordering.
class POrdStruct p a | p -> a where
    -- | List of all elements in the partially ordered set.
    elementsOf :: p -> [a]
    -- | Partial order relation in poset @p@.
    lessIn     :: p -> a -> a -> Bool
    -- | Equality in order relation. Default implementation
    -- is provided.
    equalIn    :: p -> a -> a -> Bool
    equalIn poset a b = lessIn poset a b && lessIn poset b a
    -- | Least upper bound (supremum) of two elements in the poset.
    -- 'Nothing' if there is no /least/ upper bound. See 'lubIn'.
    -- Default implementation use ordering, but in specific
    -- instances there might more efficient way to compute sup.
    supIn      :: p -> a -> a -> Maybe a
    -- | Greatest lower bound (infimum) of two elements in the poset.
    -- 'Nothing' if there is no /greatest/ upper bound. See 'glbIn'.
    -- Default implementation use ordering, but in specific
    -- instances there might more efficient way to compute sup.
    infIn      :: p -> a -> a -> Maybe a
    supIn poset a b
        | length lub == 1 = Just $ head lub
        | otherwise = Nothing
        where
            lub = lubIn poset [a, b]
    infIn poset a b
        | length glb == 1 = Just $ head glb
        | otherwise = Nothing
        where
            glb = glbIn poset [a, b]

-- | Partialy ordered types.
class POrd a where
    (.<=.) :: a -> a -> Bool
    (.>=.) :: a -> a -> Bool
    equiv  :: a -> a -> Bool

    (.>=.) = flip (.<=.)
    equiv a b = a .<=. b && b .<=. a

infix 4 .<=.
infix 4 .>=.

-- |
-- = Binary operations on elements.

-- | Least upper bounds of list of elements,
-- i.e. minimal elements in the set of elements
-- that are greater than all.
lubIn :: POrdStruct p a => p -> [a] -> [a]
lubIn poset as = minimalIn poset $ lubIn' poset as (elementsOf poset)

lubIn' _ [] pool = pool
lubIn' poset (a:as) pool = lubIn' poset as $ filter (a ≤) pool
    where
        (≤) = lessIn poset

-- | Set of greatest lower bounds of two elements
-- i.e. maximal elements in the set of elements
-- that are less than both.
glbIn :: POrdStruct p a => p -> [a] -> [a]
glbIn poset as = maximalIn poset $ glbIn' poset as (elementsOf poset)

glbIn' _ [] pool = pool
glbIn' poset (a:as) pool = glbIn' poset as (filter (≤ a) pool)
    where
        (≤) = lessIn poset

-- | Unsafe least upper bound (assumes that it exists).
unsafeSupIn :: POrdStruct p a => p -> a -> a -> a
unsafeSupIn poset a b = fromJust $ supIn poset a b

-- | Unsafe greatest lower bound (assumes that it exists).
unsafeInfIn :: POrdStruct p a => p -> a -> a -> a
unsafeInfIn poset a b = fromJust $ infIn poset a b

-- | List of elements that are greater or equal than given one
geEqThan :: POrdStruct p a => p -> a -> [a]
geEqThan poset a = filter (lessIn poset a) $ elementsOf poset

-- |List of elements that are greater or equal than given one
leEqThan :: POrdStruct p a => p -> a -> [a]
leEqThan poset a = filter (\b -> lessIn poset b a) $ elementsOf poset

-- | Returns minimal elements in a given list,
-- i.e. elements that are not greater than any of the
-- other elements of the set.
minimalIn :: POrdStruct p a => p -> [a] -> [a]
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
maximalIn :: POrdStruct p a => p -> [a] -> [a]
maximalIn _ [] = []
maximalIn _ [a] = [a]
maximalIn poset (a:as)
    | any (a ≤) as = maximalIn poset as
    | otherwise = a:(maximalIn poset $ filter (not . (≤ a)) as)
    where
        (≤) = lessIn poset

-- |
-- == Testing various properties

-- | Property O2 from general construction (see old preprint) OBSOLETE
propertyO2 :: POrdStruct p a => p -> (a -> a -> Maybe a) -> Bool
propertyO2 poset op = and [a ≤ b `iff` any (\a' -> b `equal` (a `op` a')) els | a <- els, b <- els]
    where
        els = elementsOf poset
        (≤) = lessIn poset
        b `equal` ma = case ma of
                           Nothing -> False
                           Just a -> equalIn poset b a

-- | True if Poset is actually a partially ordered set
isPoset :: (Eq a, POrdStruct p a) => p -> Bool
isPoset poset = isReflexive els rel && isAntiSymmetric els rel && isTransitive els rel
    where
        els = elementsOf poset
        rel = Function $ lessIn poset

-- | Pretty prints the poset
showPoset :: (Show a) => POrdStruct p a => p -> String
showPoset poset = "No. of elements: " ++ (show $ length $ elementsOf poset) ++
                  "\nGreater than lists:\n" ++ (unlines $ gtlists)
          where
              gtlists = map (\a -> (show a) ++ "| " ++ (show $ geEqThan poset a)) $ elementsOf poset
