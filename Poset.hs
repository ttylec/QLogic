{-# LANGUAGE GADTs, BangPatterns #-}
module Poset where
   
import Debug.Trace

import Control.Monad.Identity
import Data.List
import Data.Maybe

import QLogic
import Data.Array.Repa hiding ((++), map)
import qualified Data.Vector as V
import qualified Data.IntSet as IntSet

type Relation = Array U DIM2 Bool

relationFromFunction :: V.Vector a -> (a -> a -> Bool) -> Relation
relationFromFunction elements rel = computeS (fromFunction (Z:.n:.n) isLess)
    where
        isLess (Z:.i:.j) = (elements V.! i) `rel` (elements V.! j)
        n = V.length elements

-- newtype Set a = V.Vector a

-- at :: Set a -> Int i -> a
-- {-# INLINE at #-}
-- set `at` i = set V.! i

fromPoset :: (Poset a) => V.Vector a -> Relation
fromPoset elements = relationFromFunction elements (.<.) 

data Equiv a = (Eq a) => Equiv [a] --deriving (Show)

instance Show a => Show (Equiv a) where
        show (Equiv a) = show a

inEquivClass :: a -> Equiv a -> Bool
a `inEquivClass` (Equiv bs) = a `elem` bs 

reprOf :: Equiv a -> a
reprOf (Equiv (a:_)) = a

equivReprIndex :: V.Vector (Equiv a) -> a -> Int
equivReprIndex set a = fromJust $ V.findIndex (a `inEquivClass`) set

quotientSet :: (Eq a) => (V.Vector a, Relation) -> (V.Vector (Equiv a), Relation)
quotientSet (elements, rel) = (qel, qrel)
    where
        n = V.length elements
        qel = V.fromList $ [Equiv [elements V.! i | i <- is] | is <- (reduceByRelation [] rel [0..n-1])]
        qrel = relationFromFunction qel (qLess)
        qLess (Equiv (a:_)) (Equiv (b:_)) = rel ! (Z:.i:.j)
            where
                i = fromJust $ V.elemIndex a elements
                j = fromJust $ V.elemIndex b elements
                                                       
reduceByRelation accum _ [] = accum
reduceByRelation accum rel es@(e:_) = reduceByRelation (eq:accum) rel $ not_eq
    where
        (eq, not_eq) = partition (eqRel rel e) es

eqRel :: Relation -> Int -> Int -> Bool
eqRel rel i j = (rel ! (Z:.i:.j)) && (rel ! (Z:.j:.i))

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

rJoin :: Relation -> Int -> Int -> Maybe Int
rJoin rel i j 
    | length mins == 1 = Just $ head mins
    | otherwise = Nothing
    where
        mins = rMin rel [k | k <- [0..n-1], i `less` k, j `less` k]
        Z :. _ :. n = extent rel
        less i j = rel ! (Z:.i:.j)

rMeet :: Relation -> Int -> Int -> Maybe Int
rMeet rel i j
    | length maxs == 1 = Just $ head maxs
    | otherwise = Nothing
    where
        maxs = rMax rel [k | k <- [0..n-1], k `less` i, k `less` j] 
        Z :. _ :. n = extent rel
        less i j = rel ! (Z:.i:.j)

rMin :: Relation -> [Int] -> [Int]
rMin _ [] = []
rMin _ [a] = [a]
rMin rel (a:as)
    | any (`less` a) as = rMin rel as
    | otherwise = a:(rMin rel $ filter (not . (a `less`))  as)
    where
        less i j = rel ! (Z:.i:.j)

rMax :: Relation -> [Int] -> [Int]
rMax _ [] = []
rMax _ [a] = [a]
rMax rel (a:as)
    | any (a `less`) as = rMax rel as
    | otherwise = a:(rMax rel $ filter (not . (`less` a)) as)
    where
        less i j = rel ! (Z:.i:.j)

data BoxProduct a b = (FiniteLogic a, FiniteLogic b) => BoxProduct (V.Vector (Equiv (FreeProduct a b)), Relation)

boxProduct :: (FiniteLogic a, FiniteLogic b) => [a] -> [b] -> BoxProduct a b
boxProduct as bs = BoxProduct (boxPropositions, boxOrder)
    where
        boxQ = V.fromList $ boxQuestions as bs
        boxPreOrder = transitiveClosure $ relationFromFunction boxQ boxPrec
        (boxPropositions, boxOrder) = quotientSet (boxQ, boxPreOrder)

boxElements :: BoxProduct a b -> [Equiv (FreeProduct a b)]
boxElements (BoxProduct (els, _)) = V.toList els

boxJoin :: BoxProduct a b -> FreeProduct a b -> FreeProduct a b -> Maybe (FreeProduct a b)
boxJoin (BoxProduct (els, rel)) a b = liftM (reprOf . (els V.!)) $ rJoin rel i j 
    where
        i = equivReprIndex els a
        j = equivReprIndex els b

boxLess :: BoxProduct a b -> FreeProduct a b -> FreeProduct a b -> Bool
boxLess (BoxProduct (els, rel)) a b = rel ! (Z:.i:.j)
    where
        i = equivReprIndex els a
        j = equivReprIndex els b

boxGreater :: BoxProduct a b -> FreeProduct a b -> FreeProduct a b -> Bool
boxGreater box = flip (boxLess box)

-- boxOrtho :: BoxProduct a b -> FreeProduct a b -> Maybe (FreeProduct a b)
boxOrtho (BoxProduct (els, rel)) a  -- = (n, V.length els, orthos) 
    | length maxorthos == 1 = Just $ reprOf $ (els V.!) $ head maxorthos
    | otherwise = Nothing
    where
        maxorthos = rMax rel orthos
        orthos = [i | i <- [0..n-1], (reprOf $ els V.! i) `boxOrthogonal` a]
        Z :. _ :. n = extent rel

-- minimal :: Poset a -> [a] -> [a]
-- minimal _ [] = []
-- minimal _ [a] = [a]
-- minimal poset (a:as) 
--     | any (\b -> lessEq poset b a) as = minimal poset as
--     | otherwise = a:(minimal poset $ filter (not . (lessEq poset a)) as)

ex_rel1 = fromListUnboxed (Z:.(3::Int):.(3::Int))  [True, True, True, 
                                                   True, True, False, 
                                                   False, True, True]

ex_rel2 = fromListUnboxed (Z:.(3::Int):.(3::Int))  [True, True, True, 
                                                   False, True, True, 
                                                   True, False, True]

ex_rel3 = fromListUnboxed (Z:.(3::Int):.(3::Int))  [True, True, False, 
                                                   False, True, True, 
                                                   False, False, True]

data FreeProduct a b = FreeProd a b | FreePlus (FreeProduct a b) (FreeProduct a b)

instance (Show a, Show b) => Show (FreeProduct a b) where
        show (FreeProd a b) = (show a) ++ (show b)
        show (FreePlus a p) = (show a) ++ "⊕" ++ (show p)

instance (Repr a, Repr b) => Repr (FreeProduct a b) where
        repr (FreeProd a b) = (repr a) ++ (repr b)
        repr (FreePlus a p) = (repr a) ++ "plus" ++ (repr p)

instance (Poset a, Poset b) => Eq (FreeProduct a b) where
        a == b = a .<. b && b .<. a

instance (Poset a, Poset b) => Poset (FreeProduct a b) where
        (FreeProd a1 a2) .<. (FreeProd b1 b2) = a1 .<. b1 && a2 .<. b2
        a@(FreeProd _ _) .<. (FreePlus b bs) = a .<. b || a .<. bs
        (FreePlus a as) .<. b = a .<. b && as .<. b

(<>) :: (Logic a, Logic b) => a -> b -> FreeProduct a b
(<>) a b
    | (a == zero) || (b == zero) = FreeProd zero zero
    | otherwise = FreeProd a b

(<+>) :: (Logic a, Logic b) => FreeProduct a b -> FreeProduct a b -> FreeProduct a b
(<+>) a@(FreeProd _ _) b = FreePlus a b
(<+>) a@(FreePlus _ _) b@(FreeProd _ _) = FreePlus b a
(<+>) a@(FreePlus a1 a2) b@(FreePlus _ _) = a1 <+> (a2 <+> b)

freeToList :: FreeProduct a b -> [FreeProduct a b]
freeToList a@(FreeProd _ _) = [a]
freeToList (FreePlus a b) = a:(freeToList b)

freeFromList :: (Logic a, Logic b) => [FreeProduct a b] -> FreeProduct a b
freeFromList alist = foldl1 (<+>) alist

boxQuestions :: (FiniteLogic a, FiniteLogic b) => [a] -> [b] -> [FreeProduct a b]
boxQuestions ela elb = nub $ zerozero:oneone:others 
        where
            zerozero = zero <> zero
            oneone = one <> one
            others = concat $ takeWhile ((> 1) . length) $ iterate (makeAdmissibleSums pairs) pairs 
            pairs = [a <> b | a <- ela, b <- elb, a /= zero, b /= zero]

makeAdmissibleSums [] qs = []
makeAdmissibleSums (p:ps) qs = (makeAdmissibleSums' p qs) ++ (makeAdmissibleSums ps qs)
makeAdmissibleSums' p qs = map (p <+>) $ filter (boxOrthogonal p) qs

boxOrthogonal :: (FiniteLogic a, FiniteLogic b) => FreeProduct a b -> FreeProduct a b -> Bool
boxOrthogonal (FreeProd a1 a2) (FreeProd b1 b2) = (a1 -|- b1) || (a2 -|- b2)
boxOrthogonal a@(FreeProd _ _) (FreePlus b bs) = (a `boxOrthogonal` b) && (a `boxOrthogonal` bs)
boxOrthogonal (FreePlus a as) b = (a `boxOrthogonal` b) && (as `boxOrthogonal` b)

rightOf :: (FiniteLogic a, FiniteLogic b) => a -> FreeProduct a b -> FreeProduct a b
rightOf c a 
    | c /= zero = c <> (rightOf' zero c a)
    | otherwise = c <> one

rightOf' :: (FiniteLogic a, FiniteLogic b) => b -> a -> FreeProduct a b -> b
rightOf' accum c (FreeProd a b)
    | a .>. c = accum \./ b
    | otherwise = accum
rightOf' accum c (FreePlus a as) = rightOf' (rightOf' accum c a) c as

leftOf :: (FiniteLogic a, FiniteLogic b) => b -> FreeProduct a b -> FreeProduct a b
leftOf d a 
    | d /= zero = (leftOf' zero d a) <> d
    | otherwise = one <> d

leftOf' :: (FiniteLogic a, FiniteLogic b) => a -> b -> FreeProduct a b -> a
leftOf' accum d (FreeProd a b)
    | b .>. d = accum \./ a
    | otherwise = accum
leftOf' accum d (FreePlus a as) = leftOf' (leftOf' accum d a) d as

boxPrec :: (FiniteLogic a, FiniteLogic b) => FreeProduct a b -> FreeProduct a b -> Bool
boxPrec (FreeProd a b) (FreeProd c d) = (a .<. c) && (b .<. d)
boxPrec ab@(FreeProd a b) p = (ab `boxPrec` (leftOf b p)) || (ab `boxPrec` (rightOf a p))
boxPrec (FreePlus a as) p = (a `boxPrec` p) && (as `boxPrec` p)

-- 
-- 
-- data RawPoset a where
--         RawPoset :: Packed a -> Relation -> RawPoset a
-- 
-- lessEq :: Poset a -> a -> a -> Bool
-- lessEq poset@(Poset _ _) a b = Set.member b $ greaterThan poset a
-- 
-- greaterEq :: Poset a -> a -> a -> Bool
-- greaterEq poset = flip (lessEq poset)
-- 
-- greaterThan :: Poset a -> a -> Set.Set a
-- greaterThan (Poset _ rel) a = case Map.lookup a rel of
--                                   Just glist -> glist
--                                   Nothing -> Set.fromList []
-- 
-- lub :: Poset a -> a -> a -> Maybe a
-- lub poset@(Poset _ _) a b
--     | length mins == 1 = Just $ head mins
--     | otherwise = Nothing
--     where
--        mins = minimal poset $ Set.toList $ Set.intersection (greaterThan poset a) (greaterThan poset b) 
-- 
-- minimal :: Poset a -> [a] -> [a]
-- minimal _ [] = []
-- minimal _ [a] = [a]
-- minimal poset (a:as) 
--     | any (\b -> lessEq poset b a) as = minimal poset as
--     | otherwise = a:(minimal poset $ filter (not . (lessEq poset a)) as)

-- glb :: Poset a -> a -> a -> Maybe a
-- glb (Poset _ rel) a b = 
--
-- Utilities and examples
--
--
-- subsetRelation sets = foldl go Map.empty sets
--     where
--         go acc a = Map.insert a (glist a) acc
--         glist a = Set.fromList $ filter (a `isSubset`) sets
-- 
-- spaceN :: Int -> Poset [Int]
-- spaceN n = Poset sets (subsetRelation sets)
--     where
--         sets = subsets [1..n]
-- 
-- isSubset [] b = True
-- isSubset (a:as) b = (a `elem` b) && isSubset as b
-- 
-- subsets :: [Int] -> [[Int]]
-- subsets []  = [[]]
-- subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)
