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
import Control.Parallel.Strategies
import Debug.Trace

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
--       such that a_i ≤ ortho a_j for i /= j, supremum
--       of [a_1 ..] is an element of L.
--  (L5) orthomodular law: a ≤ b implies b = a \/ (b /\ ortho a)
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
       PackedQLogic :: Int -> Relation -> OrthoMap -> Int -> Int -> QLogic Int

instance (Show a) => Show (QLogic a) where
        show ql@(QLogic _ _ _ _ _) = "elements:\n" ++ (show $ els) ++ "\n" ++
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

-- | Convert QLogic to PackedQLogic.
packQLogic :: QLogic a -> QLogic Int
packQLogic (QLogic els rel ocmpl zero one) = 
        PackedQLogic n prel pocmpl (toKey packed zero) (toKey packed one)
        where
            n = length els
            packed = packList els
            prel = relationFromFunction n $ packRel packed rel 
            pocmpl = orthoMapFromFunction n $ pack packed ocmpl

-- TODO write this function
-- unpackQLogic :: [a] -> QLogic Int -> QLogic a
-- unpackQLogic 

-- = Getting elements
-- | List of elements in quantum logic
elementsOf :: QLogic a -> [a]
elementsOf (QLogic els _ _ _ _) = els
elementsOf (PackedQLogic n _ _ _ _) = [0..n-1]

-- | Order relation in quantum logic
lessIn :: QLogic a -> (a -> a -> Bool)
lessIn (QLogic _ rel _ _ _) = rel
lessIn (PackedQLogic _ rel _ _ _) = \i j -> rel ! (Z:.i:.j)

equalIn :: QLogic a -> (a -> a -> Bool)
equalIn ql a b = lessIn ql a b && lessIn ql b a

-- | Orthocompletion in quantum logic
ocmplIn :: QLogic a -> (a -> a)
ocmplIn (QLogic _ _ omap _ _) = omap
ocmplIn (PackedQLogic _ _ omap _ _) = \i -> omap ! (Z:.i)

-- | The least element of quantum logic
zeroOf :: QLogic a -> a
zeroOf (QLogic _ _ _ z _) = z
zeroOf (PackedQLogic _ _ _ z _) = z

-- | The greatest element of quantum logic
oneOf :: QLogic a -> a
oneOf (QLogic _ _ _ _ o) = o
oneOf (PackedQLogic _ _ _ _ o) = o

-- = Basic quantum logic structure
--
-- | List of atoms in logic,
-- i.e. elements covering zero.
atomsOf :: QLogic a -> [a]
atomsOf ql = minimalIn ql $ filter (not . (equalIn ql z)) $ elementsOf ql
    where
        z = zeroOf ql

-- | List of elements in quantum logic that are greater than given element
-- TODO this we could specialize for PackedQLogic, but we don't use it
-- anywas in any performance critical part
greaterThan :: QLogic a -> a -> [a]
greaterThan ql a = filter (lessIn ql a) $ elementsOf ql

-- | Set of least upper bounds of two elements,
-- i.e. minimal elements in the set of elements
-- that are greater than both.
lubIn :: QLogic a -> a -> a -> [a] 
lubIn ql a b = minimalIn ql $ filter (a ≤) $ filter (b ≤) els
    where
        els = elementsOf ql
        (≤) = lessIn ql

-- | Set of greatest lower bounds of two elements
-- i.e. maximal elements in the set of elements
-- that are less than both.
glbIn :: QLogic a -> a -> a -> [a] 
glbIn ql a b = maximalIn ql $ filter (≤ a) $ filter (≤ b) els
    where
        els = elementsOf ql
        (≤) = lessIn ql

-- |Returns true if given two elements are orthogonal in set
orthoIn :: QLogic a -> a -> a -> Bool
orthoIn ql p q = lessIn ql p (ocmplIn ql q)

-- = Partialy ordered set operations
--
-- |Lowest upper bound of pair of elements (join).
supIn :: QLogic a -> a -> a -> Maybe a
supIn ql a b  
    | length lub == 1 = Just $ head lub
    | otherwise = Nothing
    where
        lub = lubIn ql a b

-- |Unsafe lowest upper bound: assumes that it exists.
unsafeSupIn :: QLogic a -> a -> a -> a
unsafeSupIn ql a b = fromJust $ supIn ql a b

-- |Lowest upper bound of pair of elements (join).
infIn :: QLogic a -> a -> a -> Maybe a
infIn ql a b  
    | length glb == 1 = Just $ head glb
    | otherwise = Nothing
    where
        glb = glbIn ql a b

-- |Unsafe lowest upper bound: assumes that it exists.
unsafeInfIn :: QLogic a -> a -> a -> a
unsafeInfIn ql a b = fromJust $ infIn ql a b

-- |Returns subset of minimal elements in given set,
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

-- |Returns subset of maximal elements in given set,
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

-- = Axioms of quantum logic

-- |Checks if given structure is a quantum logic
checkLogic :: (Eq a) => QLogic a -> Bool
checkLogic set = and [checkOrderReverse set, 
                     checkOrthoIdempotence set, 
                     checkSupremum set, 
                     checkOrthomodular set]

-- |Checks L2 axiom of logic.
checkOrderReverse :: QLogic a -> Bool
checkOrderReverse ql = and cond
    where
        cond = map id [(ocmpl q) .<. (ocmpl p) | p <- elementsOf ql, q <- elementsOf ql, p .<. q] `using` parList rdeepseq
        (.<.) = lessIn ql
        ocmpl = ocmplIn ql

-- |Check L3 axiom in given set of elements
checkOrthoIdempotence :: (Eq a) => QLogic a -> Bool
checkOrthoIdempotence ql = and cond 
    where
        cond = map idem (elementsOf ql) `using` parList rdeepseq 
        idem p = p == (ocmpl . ocmpl $ p)
        ocmpl = ocmplIn ql

-- |Checks L4 axiom in given set of elements
checkSupremum :: (Eq a) => QLogic a -> Bool
checkSupremum ql = all (/= Nothing) orthosups
    where
        orthosups = map orthosup (elementsOf ql) `using` parList rseq
        orthosup p = foldM sup p [q | q <- elementsOf ql, orthoIn ql p q]
        sup = supIn ql

-- |Checks L5 axiom in given set of elements
checkOrthomodular :: (Eq a) => QLogic a -> Bool
checkOrthomodular ql = and cond
    where
        cond = map id [Just b == rhs a b | a <- set, b <- set, a .<. b] `using` parList rdeepseq
        rhs a b = (inf b $ ocmpl a) >>= (sup a)
        (.<.) = lessIn ql
        inf = infIn ql
        sup = supIn ql
        ocmpl = ocmplIn ql
        set = elementsOf ql

-- |Checks if two elements are compatible in given subset,
-- i.e. a and b are compatible in K whenever there exists
-- a_1, b_1 and c such that:
--
--      a = a1 \/ c     (1a)
--      b = a2 \/ c     (1b)
--
--  and [a1, a2, c] is mutually orthogonal collection
--  of elements.
--
--  Implementation is basically the proof of Proposition 1.3.5 of [1]:
--
--  Assume that a b are compatible. Then: 
--
--      a1 = a /\ ortho b, b1 = b /\ ortho a, c = a /\ b
--
--  (all exists by the assumption), and a1, b1, c are mutually
--  orthogonal, and satisfy (1a) and (1b). If any of these steps fail
--  we showed that a and b cannot be compatible (by contradiction).
--
--  Contrary, if above procedure resulted in True, we showed explicitly
--  that a and b are compatible.
compatibleIn :: (Eq a) => QLogic a -> a -> a -> Bool
compatibleIn ql a b = all (fromMaybe False) [are_ortho, are_equal_a, are_equal_b]
    where 
          are_equal_a = liftM2 (==) aa $ Just a
          are_equal_b = liftM2 (==) bb $ Just b
          are_ortho = liftM (mutuallyOrthogonalIn ql) $ sequence [a1, b1, c] 
          c = infIn ql a b
          a1 = infIn ql a $ ocmplIn ql b
          b1 = infIn ql b $ ocmplIn ql a
          aa = join $ liftM2 (supIn ql) a1 c
          bb = join $ liftM2 (supIn ql) b1 c

-- |Checks if elemensts in given subset are mutually orthogonal in set
mutuallyOrthogonalIn :: (Eq a) => QLogic a -> [a] -> Bool
mutuallyOrthogonalIn ql = mutuallyBy (orthoIn ql)

-- |Checks if elements in given subset are mutually orthogonal in set
mutuallyCompatibleIn :: (Eq a) => QLogic a -> [a] -> Bool
mutuallyCompatibleIn ql = mutuallyBy (compatibleIn ql)

-- |Utitlity function to perform "mutuall" tests.
mutuallyBy :: (a -> a -> Bool) -> [a] -> Bool
mutuallyBy _ [] = True
mutuallyBy f (a:as) = (all (f a) as) && mutuallyBy f as

checkBoolean :: (Eq a) => QLogic a -> Bool
checkBoolean ql = mutuallyCompatibleIn ql $ elementsOf ql

-- = Auxialliary data structures

-- |Relation represented as repaDIM2 array
type Relation = Array U DIM2 Bool

relationFromFunction :: Int -> (Int -> Int -> Bool) -> Relation
relationFromFunction n rel = runIdentity $ computeUnboxedP (fromFunction (Z:.n:.n) isLess)
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
