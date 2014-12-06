{-# LANGUAGE GADTs, BangPatterns #-}

module Data.QLogic where
   
import Prelude hiding (zipWith)

import Control.Monad
import Control.Monad.Identity
import Data.Maybe
import Data.Array.Repa hiding ((++), map)
import qualified Data.Array.Repa as Repa
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as Map

-- = TODO
--  * extend documentation in second part

-- |Class for partialy ordered elements
class (Eq a, Ord a) => POrd a where
        (≤) :: a -> a -> Bool
        (≥) :: a -> a -> Bool
        equiv :: a -> a -> Bool

        (≥) = flip (≤)
        equiv a b = a ≤ b && b ≤ a

infix 4 ≤
infix 4 ≥

-- | Data type for quantum logics.
--
-- We assume that we work only with finite logics,
-- so without loss of generality we require that
-- there is a total order on the elements of logic.
-- This order is /different/ from order of logic
-- and is used only for optimization.
--
-- Quantum logic is defined as a set with partial order
-- relation ≤ and orthocompletion ortho such that:
--
--  (L1) there exists least and greatest (distinct) elements in L
--  (L2) a <= b implies ortho a >= ortho b
--  (L3) ortho . ortho = id
--  (L4) for any countable family [a_1 ..] of elements of L,
--       such that a_i <= ortho a_j for i /= j, supremum
--       of [a_1 ..] is an element of L.
--  (L5) orthomodular law: a <= b implies b = a \/ (b /\ ortho a)
--       (we implicitly assume that the above exists)
--
-- The constructor of quantum logic does not check these
-- axioms. One need to check them independently. Any function
-- that takes quantum logic as an argument also assumes that
-- the above axioms are satisfied.
--
-- > QLogic elements partialOrderRelation orthocompletion min max
data QLogic a where
       QLogic :: (Ord a) => [a] -> (a -> a -> Bool) -> (a -> a) -> a -> a -> QLogic a

instance (Show a) => Show (QLogic a) where
        show ql = "elements:\n" ++ (show $ els) ++ "\n" ++
            "\ngreater than lists:\n" ++ (unlines $ gtlists) ++
            "\northocompletions:\n" ++ (unlines $ olists)
            where
                gtlists = map (\a -> (show a) ++ "| " ++ (show $ greaterThan ql a)) els
                olists = map (\a -> (show a) ++ "C = " ++ (show $ omap a)) els
                els = elementsOf ql
                rel = lessIn ql
                omap = ocmplIn ql

-- | Construct quantum logic from the list of partialy ordered elements. 
-- Orthocompletion map must be given explicitly.
fromPOrd :: (Eq a, POrd a) => [a] -> (a -> a) -> QLogic a
fromPOrd els omap = qlogic
    where
        qlogic = QLogic els (≤) omap min max
        min = head $ minimalIn qlogic els
        max = head $ maximalIn qlogic els

fromList :: (Ord a) => [a] -> (a -> a -> Bool) -> (a -> a) -> QLogic a
fromList els order omap = qlogic 
    where
        qlogic = QLogic els order omap min max
        min = head $ minimalIn qlogic els
        max = head $ maximalIn qlogic els

-- = Getting elements
-- | List of elements in quantum logic
elementsOf :: QLogic a -> [a]
elementsOf (QLogic els _ _ _ _) = els

-- | Order relation in quantum logic
lessIn :: QLogic a -> (a -> a -> Bool)
lessIn (QLogic _ rel _ _ _) = rel

equalIn :: QLogic a -> (a -> a -> Bool)
equalIn ql a b = lessIn ql a b && lessIn ql b a

-- | Orthocompletion in quantum logic
ocmplIn :: QLogic a -> (a -> a)
ocmplIn (QLogic _ _ omap _ _) = omap

-- | The least element of quantum logic
zeroOf :: QLogic a -> a
zeroOf (QLogic _ _ _ z _) = z

-- | The greatest element of quantum logic
oneOf :: QLogic a -> a
oneOf (QLogic _ _ _ _ o) = o

-- = Quering quantum logic
--
-- | List of atoms in logic,
-- i.e. elements covering zero.
atomsOf :: QLogic a -> [a]
atomsOf ql = minimalIn ql $ filter (not . (equalIn ql z)) $ elementsOf ql
    where
        z = zeroOf ql

-- | List of elements in quantum logic that are greater than given element
greaterThan :: QLogic a -> a -> [a]
greaterThan ql a = filter (lessIn ql a) $ elementsOf ql

-- |Returns true if given two elements are orthogonal in set
orthoIn :: QLogic a -> a -> a -> Bool
orthoIn ql p q = lessIn ql p (ocmplIn ql q)

-- = Partialy ordered set operations

-- | Lowest upper bound of pair of elements (join).
supIn :: QLogic a -> a -> a -> Maybe a
supIn ql a b  
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
        lub = minimalIn ql $ filter (b ≤) $ filter (a ≤) $ elementsOf ql
        (≤) = lessIn ql

-- | Unsafe lowest upper bound: assumes that it exists.
unsafeSupIn :: QLogic a -> a -> a -> a
unsafeSupIn ql a b = fromJust $ supIn ql a b

-- | Returns subset of minimal elements in given set,
-- i.e. elements that are not greater than any of the
-- other elements of the set.
minimalIn :: QLogic a -> [a] -> [a]
minimalIn _ [] = []
minimalIn _ [a] = [a]
minimalIn ql (a:as)
    | any (≤ a) as = minimalIn ql as
    | otherwise = a:(minimalIn ql $ filter (not . (a ≤)) as)
    where
        (≤) = lessIn ql

-- | Returns subset of maximal elements in given set,
-- i.e. element that are not less than any of the
-- other lements of the set.
maximalIn :: QLogic a -> [a] -> [a]
maximalIn _ [] = []
maximalIn _ [a] = [a]
maximalIn ql (a:as)
    | any (a ≤) as = maximalIn ql as
    | otherwise = a:(maximalIn ql $ filter (not . (≤ a)) as)
    where
        (≤) = lessIn ql

-- = Auxialliary data structures

-- | Data type for more efficient implementation of
-- quantum logic structure. Elements are represented
-- by positive integer numbers. Relation is represented
-- by a binary Repa array and orthocompletion is
-- DIM1 Repa array.
data PQLogic = PQLogic Int Relation OrthoMap deriving (Show)

-- | Convert 'QLogic' to 'PQLogic'.
-- Returns also 'Packed' data type that can be
-- used to convert between elements of QLogic 
-- and integers used by PQLogic.
packQLogic :: (Ord a) => QLogic a -> (Packed a, PQLogic)
packQLogic ql = (idx, PQLogic n pRel pOmap)
    where
        els = elementsOf ql
        rel = lessIn ql
        omap = ocmplIn ql
        n = length els
        idx = packList els
        pRel = relationFromFunction n (packRel idx rel)
        pOmap = orthoMapFromFunction n (pack idx omap)

packedGreaterThan :: PQLogic -> Int -> [Int]
packedGreaterThan (PQLogic _ rel _) i = whereIsTrue $ computeUnboxedS $ rel `inRightRelationTo` i

packedSupIn :: PQLogic -> Int -> Int -> Maybe Int
packedSupIn (PQLogic _ rel _) i j
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
        upper = whereIsTrue $ computeUnboxedS $ zipWith (&&) (rel `inRightRelationTo` i) (rel `inRightRelationTo` j)
        lub = filter (isMinimalInRel rel upper) upper

isMinimalInRel rel is i = not $ any (`less` i) is
    where
        less i j | i == j = False
                 | otherwise = rel ! (Z:.i:.j)

--
-- Auxilliary data structures
--

-- |Relation represented as repaDIM2 array
type Relation = Array U DIM2 Bool

inRightRelationTo :: Relation -> Int -> Array D DIM1 Bool
inRightRelationTo rel i = slice rel (Z:.i:.All)

inLeftRelationTo :: Relation -> Int -> Array D DIM1 Bool
inLeftRelationTo rel i = slice rel (Z:.All:.i)

whereIsTrue :: Array U DIM1 Bool -> [Int]
whereIsTrue arr = VU.ifoldl' isTrue [] $ toUnboxed $ arr
    where
        isTrue accum j True = j:accum
        isTrue accum _ False = accum

relationFromFunction :: Int -> (Int -> Int -> Bool) -> Relation
relationFromFunction n rel = computeUnboxedS (fromFunction (Z:.n:.n) isLess)
    where
        isLess (Z:.i:.j) = rel i j

relationFromFunctionP :: Int -> (Int -> Int -> Bool) -> Relation
relationFromFunctionP n rel = runIdentity $ computeUnboxedP (fromFunction (Z:.n:.n) isLess)
    where
        isLess (Z:.i:.j) = rel i j

relLess :: Relation -> Int -> Int -> Bool
relLess rel i j = rel ! (Z:.i:.j)

relEquiv :: Relation -> Int -> Int -> Bool
relEquiv rel i j = (rel ! (Z:.i:.j)) && (rel ! (Z:.j:.i))

transitiveClosure :: Relation -> Relation
transitiveClosure rel = runIdentity $ go rel 0
    where
        Z :. _ :. n = extent rel
        go !g !k 
            | k == n    = return g
            | otherwise = do
                g' <- computeP (fromFunction (Z:.n:.n) sp)
                go g' (k+1)
                where
                    sp (Z:.i:.j) = (g ! (Z:.i:.j)) || ((g ! (Z:.i:.k)) && (g ! (Z:.k:.j)))


-- |Orthocomplement map represented as repa DIM1 array
type OrthoMap = Array U DIM1 Int

orthoMapFromFunction :: Int -> (Int -> Int) -> OrthoMap
orthoMapFromFunction n omap = computeUnboxedS (fromFunction (Z:.n) ortho)
    where
        ortho (Z:.i) = omap i

-- |Dictonary for "packing" abstract structures,
-- for optimized operations using repa arrays.
data Packed a = Packed (Map.Map a Int) (V.Vector a)

packList :: (Ord a) => [a] -> Packed a
packList els = Packed dict vals
    where
        vals = V.fromList els
        dict = Map.fromList $ zip els [0..]

toKey :: (Ord a) => Packed a -> a -> Int
toKey (Packed dict _) a = case Map.lookup a dict of
                                  Nothing -> error "No such element"
                                  Just i -> i

fromKey:: (Ord a) => Packed a -> Int -> a
fromKey (Packed _ dict) i = dict V.! i

packValues (Packed _ vals) = V.toList vals 

unpack :: (Ord a) => Packed a -> (Int -> Int) -> a -> a
unpack idx f a = fromKey idx $ f (toKey idx a)

unpackM2 :: (Ord a, Monad m) => Packed a -> (Int -> Int -> m Int) -> a -> a -> m a
unpackM2 idx f a b = liftM (fromKey idx) $ f (toKey idx a) (toKey idx b)

unpack2 :: (Ord a) => Packed a -> (Int -> Int -> Int) -> a -> a -> a
unpack2 idx f a b = fromKey idx $ f (toKey idx a) (toKey idx b)

unpackRel :: (Ord a) => Packed a -> (Int -> Int -> Bool) -> a -> a -> Bool 
unpackRel idx f a b = f (toKey idx a) (toKey idx b)

pack :: (Ord a) => Packed a -> (a -> a) -> Int -> Int
pack idx f a = toKey idx $ f (fromKey idx a)

packRel :: (Ord a) => Packed a -> (a -> a -> Bool) -> Int -> Int -> Bool
packRel idx f i j = f (fromKey idx i) (fromKey idx j)
