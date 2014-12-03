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
-- > QLogic elements partialOrderRelation orthocompletion
data QLogic a where
       QLogic :: (Ord a) => [a] -> (a -> a -> Bool) -> (a -> a) -> QLogic a

instance (Show a) => Show (QLogic a) where
        show omp@(QLogic els rel omap) = "elements:\n" ++ (show $ els) ++ "\n" ++
            "\ngreater than lists:\n" ++ (unlines $ gtlists) ++
            "\northocompletions:\n" ++ (unlines $ olists)
            where
                gtlists = map (\a -> (show a) ++ "| " ++ (show $ greaterThan omp a)) els
                olists = map (\a -> (show a) ++ "C = " ++ (show $ omap a)) els

-- | Construct quantum logic from the list of partialy ordered elements. 
-- Orthocompletion map must be given explicitly.
fromPOrd :: (Eq a, POrd a) => [a] -> (a -> a) -> QLogic a
fromPOrd els omap = QLogic els (≤) omap

-- = Getting elements
-- | List of elements in quantum logic
elements :: QLogic a -> [a]
elements (QLogic els _ _) = els

-- | List of atoms in logic,
-- i.e. elements covering zero.
atoms :: QLogic a -> [a]
atoms ql@(QLogic els rel _) = minimalIn ql $ filter (not . (isEquivIn ql z)) els
    where
        z = zero ql

-- | The least element of quantum logic
zero :: QLogic a -> a
zero ql@(QLogic els _ _) = head $ minimalIn ql els

-- | The greatest element of quantum logic
one :: QLogic a -> a
one ql@(QLogic els _ _) = head $ maximalIn ql els

-- | List of elements in quantum logic that are greater than given element
greaterThan :: QLogic a -> a -> [a]
greaterThan (QLogic els rel _) a = filter (a `rel`) els 

-- = Quering quantum logic

-- | Tests if elements are in order in given logic
isLessIn :: QLogic a -> a -> a -> Bool
isLessIn (QLogic _ rel _) a b = a `rel` b

-- | Tests if elements are equivalent in logic
isEquivIn :: QLogic a -> a -> a -> Bool
isEquivIn (QLogic _ rel _) a b= a `rel` b && b `rel` a

-- |Returns true if given two elements are orthogonal in set
isOrthoIn :: QLogic a -> a -> a -> Bool
isOrthoIn (QLogic _ rel omap) p q = p `rel` (omap q)

-- = Partialy ordered set operations

-- | Lowest upper bound of pair of elements (join).
supIn :: QLogic a -> a -> a -> Maybe a
supIn omp@(QLogic els rel _) a b  
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
        lub = minimalIn omp $ filter (b `rel`) $ filter (a `rel`) els

-- | Unsafe lowest upper bound: assumes that it exists.
unsafeSupIn :: QLogic a -> a -> a -> a
unsafeSupIn ql a b = fromJust $ supIn ql a b

-- | Returns subset of minimal elements in given set,
-- i.e. elements that are not greater than any of the
-- other elements of the set.
minimalIn :: QLogic a -> [a] -> [a]
minimalIn _ [] = []
minimalIn _ [a] = [a]
minimalIn omp@(QLogic _ rel _) (a:as)
    | any (`rel` a) as = minimalIn omp as
    | otherwise = a:(minimalIn omp $ filter (not . (a `rel`)) as)

-- | Returns subset of maximal elements in given set,
-- i.e. element that are not less than any of the
-- other lements of the set.
maximalIn :: QLogic a -> [a] -> [a]
maximalIn _ [] = []
maximalIn _ [a] = [a]
maximalIn omp@(QLogic _ rel _) (a:as)
    | any (a `rel`) as = maximalIn omp as
    | otherwise = a:(maximalIn omp $ filter (not . (`rel` a)) as)


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
packQLogic (QLogic els rel omap) = (idx, PQLogic n pRel pOmap)
    where
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

--
-- Examples
--

data Boolean3 = BEmpty | B1 | B2 | B3 | B12 | B13 | B23 | BOne deriving (Bounded, Eq, Enum, Ord, Show)

instance POrd Boolean3 where
        BEmpty ≤ _ = True
        _ ≤ BOne = True
        B1 ≤ B12 = True
        B1 ≤ B13 = True
        B2 ≤ B12 = True
        B2 ≤ B23 = True
        B3 ≤ B13 = True
        B3 ≤ B23 = True
        _ ≤ _ = False

boolean3Logic = fromPOrd boolean3Elements boolean3Ortho

boolean3Elements :: [Boolean3]
boolean3Elements = [minBound..maxBound]

boolean3Ortho :: Boolean3 -> Boolean3
boolean3Ortho BEmpty = BOne
boolean3Ortho BOne   = BEmpty
boolean3Ortho B1     = B23
boolean3Ortho B2     = B13
boolean3Ortho B3     = B12
boolean3Ortho B12    = B3 
boolean3Ortho B13    = B2 
boolean3Ortho B23    = B1 


data Lantern = Zero | X0 | X1 | Y0 | Y1 | One deriving (Bounded, Eq, Enum, Ord, Show)

instance POrd Lantern where
        Zero ≤ _ = True
        _ ≤ One = True
        a ≤ b = a == b

lanternLogic = fromPOrd lanternElements lanternOrtho

lanternElements :: [Lantern]
lanternElements = [minBound..maxBound]

lanternOrtho :: Lantern -> Lantern
lanternOrtho Zero = One
lanternOrtho One  = Zero
lanternOrtho X0   = X1
lanternOrtho X1   = X0
lanternOrtho Y0   = Y1
lanternOrtho Y1   = Y0

(packedLanternDict, packedLantern) = packQLogic lanternLogic
