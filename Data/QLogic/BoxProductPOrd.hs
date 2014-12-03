{-# LANGUAGE GADTs, BangPatterns #-}

module Data.QLogic.BoxProductPOrd where

import Data.QLogic
import Data.List
import Data.Maybe
import Data.Monoid hiding ((<>))

-- | Free product of quantum logics. 
-- A formal object that is used in construction of box product.
-- Represents questions in product system,
-- while elements of box product represent propositions.
data FreeProduct a b = FreeProd a b | FreePlus (FreeProduct a b) (FreeProduct a b)

instance (Show a, Show b) => Show (FreeProduct a b) where
        show (FreeProd a b) = (show a) ++ (show b)
        show (FreePlus a p) = (show a) ++ "⊕" ++ (show p)

-- | Natural total order on free product is following:
instance (Ord a, Ord b) => Ord (FreeProduct a b) where
        (FreeProd a1 a2) `compare` (FreeProd b1 b2) = (a1 `compare` b1) `mappend` (a2 `compare` b2)
        (FreeProd _ _) `compare` (FreePlus _ _) = LT
        (FreePlus _ _) `compare` (FreeProd _ _) = GT
        (FreePlus a as) `compare` (FreePlus b bs) = (a `compare` b) `mappend` (as `compare` bs)

-- | Formal equality
instance (Eq a, Eq b) => Eq (FreeProduct a b) where
        (FreeProd a1 a2) == (FreeProd b1 b2) = a1 == b1 && a2 == b2
        (FreePlus a as) == (FreePlus b bs) = a == b && as == bs
        _ == _ = False

-- | Partial order on FreeProduct
instance (POrd a, POrd b) => POrd (FreeProduct a b) where
        (FreeProd a1 a2) ≤ (FreeProd b1 b2) = a1 ≤ b1 && a2 ≤ b2
        a@(FreeProd _ _) ≤ (FreePlus b bs) = a ≤ b || a ≤ bs
        (FreePlus a as) ≤ b = a ≤ b && as ≤ b

-- | Syntatic sugar for FreeProd
(<>) :: (POrd a, POrd b) => a -> b -> FreeProduct a b
(<>) a b = FreeProd a b
infixl 5 <>

-- | Should be used to construct FreePlus.
-- FreePlus is commutative by definition,
-- and for the implementation it 
-- is assumed that the order of elements
-- (w.r.t to total lexical order)
-- int the sum is asceding.
(<+>) :: (POrd a, POrd b) => FreeProduct a b -> FreeProduct a b -> FreeProduct a b
(<+>) a@(FreeProd _ _) b@(FreeProd _ _)
    | a <= b = FreePlus a b
    | otherwise = FreePlus b a
(<+>) a@(FreeProd _ _) b@(FreePlus b0 bs)
    | a <= b = FreePlus a b
    | otherwise = FreePlus b (a <+> bs)
(<+>) a@(FreePlus _ _) b@(FreeProd _ _) = b <+> a
(<+>) a@(FreePlus a1 a2) b@(FreePlus _ _) = a1 <+> (a2 <+> b)
infixl 4 <+>

-- | Returns question on left subsytem of FreeProduct
-- This is partialy defined function, matches only
-- FreeProd constructor.
leftSystem :: (POrd a, POrd b) => FreeProduct a b -> a
leftSystem (FreeProd a _) = a

-- | Returns question on right subsytem of FreeProduct.
-- This is partialy defined function, matches only
-- FreeProd constructor.
rightSystem :: (POrd a, POrd b) => FreeProduct a b -> b
rightSystem (FreeProd _ b) = b

-- | Interchanges left and right subsystems.
transposeSystems :: (POrd a, POrd b) => FreeProduct a b -> FreeProduct b a
transposeSystems (FreeProd a b) = b <> a
transposeSystems (FreePlus a as) = transposeSystems a <+> transposeSystems as

-- | Returns the list of all questions in box product of two logics.
boxQuestions :: (POrd a, POrd b) => QLogic a -> QLogic b -> [FreeProduct a b]
boxQuestions qla qlb = nub $ zerozero:oneone:others 
        where
            zerozero = zero qla <> zero qlb
            oneone = one qla <> one qlb
            ql = (qla, qlb)
            others = concat $ takeWhile ((> 1) . length) $ iterate (makeAdmissibleSums ql pairs) pairs 
            pairs = [a <> b | a <- elements qla, b <- elements qlb, a /= zero qla, b /= zero qlb]

-- | Returns the list of questions formed by atoms in box product of
-- two logics. TODO: it's not clear if this gives equivalent structure
-- as taing boxQuestions. 
boxAtomicQuestions :: (POrd a, POrd b) => QLogic a -> QLogic b -> [FreeProduct a b]
boxAtomicQuestions qla qlb = nub $ z:other
       where
           z = zero qla <> zero qlb
           ql = (qla, qlb)
           other = concat $ takeWhile ((>1) . length) $ iterate (makeAdmissibleSums ql boxAtoms) boxAtoms
           boxAtoms = [a <> b | a <- atoms qla, b <- atoms qlb]
           
makeAdmissibleSums  _ [] qs = []
makeAdmissibleSums  ql (p:ps) qs = (makeAdmissibleSums' ql p qs) ++ (makeAdmissibleSums ql ps qs)
makeAdmissibleSums' ql p qs = map (p <+>) $ filter (boxOrthogonal ql p) qs

-- | Orthogonality relation in construction of box product
boxOrthogonal :: (POrd a, POrd b) => (QLogic a, QLogic b) -> FreeProduct a b -> FreeProduct a b -> Bool
boxOrthogonal (qla, qlb) (FreeProd a1 a2) (FreeProd b1 b2) = (isOrthoIn qla a1 b1) || (isOrthoIn qlb a2 b2)
boxOrthogonal ql a@(FreeProd _ _) (FreePlus b bs) = (boxOrthogonal ql a b) && (boxOrthogonal ql a bs)
boxOrthogonal ql (FreePlus a as) b = (boxOrthogonal ql a b) && (boxOrthogonal ql as b)

rightOf :: (POrd a, POrd b) => (QLogic a, QLogic b) -> a -> FreeProduct a b -> FreeProduct a b
rightOf ql@(qla, qlb) c a
    | c /= zero qla = c <> rightOf' ql (zero qlb) c a
    | otherwise = c <> one qlb 

rightOf' (qla, qlb) accum c (FreeProd a b)
    | a ≥ c = fromJust $ supIn qlb accum b
    | otherwise = accum
rightOf' qlb accum c (FreePlus a as) = rightOf' qlb (rightOf' qlb accum c a) c as

leftOf :: (POrd a, POrd b) => (QLogic a, QLogic b) -> b -> FreeProduct a b -> FreeProduct a b
leftOf ql@(qla, qlb) d b
    | d /= zero qlb = leftOf' ql (zero qla) d b <> d
    | otherwise = one qla <> d

leftOf' (qla, qlb) accum d (FreeProd a b)
    | b ≥ d = fromJust $ supIn qla accum a
    | otherwise = accum
leftOf' qla accum d (FreePlus a as) = leftOf' qla (leftOf' qla accum d a) d as

freePlusToList :: (POrd a, POrd b) => FreeProduct a b -> [FreeProduct a b]
freePlusToList a@(FreeProd _ _) = [a]
freePlusToList (FreePlus a as) = a:(freePlusToList as)

freePlusFromList :: (POrd a, POrd b) => [FreeProduct a b] -> FreeProduct a b
freePlusFromList (a:as) = foldl' (<+>) a as

reduceRight :: (POrd a, POrd b) => QLogic b -> FreeProduct a b -> FreeProduct a b
reduceRight _ a@(FreeProd _ _) = a
reduceRight ql a@(FreePlus _ _) = freePlusFromList $ map (reduceRight' ql) groups 
    where
        groups = groupBy (\b c -> leftSystem b == leftSystem c) $ freePlusToList a

reduceRight' ql as = leftSystem (head as) <> sup rights
    where
        rights = map rightSystem as
        sup (a:as) = foldl' (unsafeSupIn ql) a as

reduceLeft :: (POrd a, POrd b) => QLogic a -> FreeProduct a b -> FreeProduct a b
reduceLeft ql = transposeSystems . (reduceRight ql) . transposeSystems

-- | Pre-order relation on box product questions.
boxPrec :: (POrd a, POrd b) => (QLogic a, QLogic b) -> FreeProduct a b -> FreeProduct a b -> Bool
boxPrec (qla, qlb) (FreeProd a b) (FreeProd c d) = a ≤ c && b ≤ d
boxPrec ql ab@(FreeProd a b) p = (boxPrec ql ab (leftOf ql b p)) || (boxPrec ql ab (rightOf ql a p))
boxPrec ql (FreePlus a as) p = (boxPrec ql a p) && (boxPrec ql as p)

-- | Pre-order relation on box product questions. OBSOLETE? 
-- This was supposed to be better for working ot atomic only questions,
-- but it seems that transitive closure takes care of that.
boxAtomicPrec :: (POrd a, POrd b) => (QLogic a, QLogic b) -> FreeProduct a b -> FreeProduct a b -> Bool
boxAtomicPrec (qla, qlb) (FreeProd a b) (FreeProd c d) =  a ≤ c && b ≤ d
boxAtomicPrec ql@(qla, qlb) ab@(FreeProd a b) p = leftLess || rightLess
    where
        leftLess = boxPrec ql ab $ leftOf ql b $ reduceRight qlb p
        rightLess = boxPrec ql ab $ rightOf ql a $ reduceLeft qla p
boxAtomicPrec ql (FreePlus a as) p = boxAtomicPrec ql a p && boxAtomicPrec ql as p

-- | BoxProduct is the equivalence class of FreeProduct's,
-- as proposition is a the equivalence class of questions.
type BoxProduct a b = Equiv (FreeProduct a b)

data Poset a where
        Poset :: (Ord a) => [a] -> (a -> a -> Bool) -> Poset a 

data PrePoset a where
        PrePoset :: (Ord a) => [a] -> (a -> a -> Bool) -> PrePoset a 

-- | Given a set with a set with preorder return the poset,
-- i.e. set of equivalence classes of preorder with order
-- induced by preorder
quotientLogic :: (POrd a) => PrePoset a -> Poset (Equiv a)
quotientLogic (PrePoset els preorder) = Poset qels order
    where
        qels = quotientBy equiv els
        order = liftRel preorder
        equiv a b = a `preorder` b && b `preorder` a 

boxProduct :: (POrd a, POrd b) => QLogic a -> QLogic b -> QLogic (BoxProduct a b)
boxProduct qla qlb = boxLogic 
    where
        boxLogic = QLogic boxElems boxLess boxOrtho
        (Poset boxElems boxLess) = quotientLogic (PrePoset questions preorder) 

        questions = boxQuestions qla qlb
        n = length questions

        preorder = unpackRel packedQs (relLess packedPreOrder)
        packedQs = packList questions
        packedPreOrder = transitiveClosure $ relationFromFunction n $ packRel packedQs $ boxPrec (qla, qlb)

        boxOrtho a = fromJust $ find (isOrtho a) boxElems 
        isOrtho a b = case boxPlus boxLogic a b of
                          Nothing -> False
                          Just c -> isEquivIn boxLogic c $ boxOne 
        boxOne = one boxLogic

boxProductRel :: (POrd a, POrd b) => QLogic a -> QLogic b -> (Packed (FreeProduct a b), Relation)
boxProductRel qla qlb = (packed, rel)
    where
        boxqs = boxQuestions qla qlb
        n = length boxqs
        packed = packList $ boxqs
        rel = transitiveClosure $ relationFromFunction n $ packRel packed (boxPrec (qla, qlb))

boxProduct' :: (POrd a, POrd b) => QLogic a -> QLogic b -> QLogic (FreeProduct a b)
boxProduct' qla qlb = QLogic els isLess id 
        where
            (packed, rel) = boxProductRel qla qlb
            els = packValues packed 
            isLess = unpackRel packed (relLess rel) 

boxAtomicProductRel :: (POrd a, POrd b) => QLogic a -> QLogic b -> (Packed (FreeProduct a b), Relation)
boxAtomicProductRel qla qlb = (packed, rel)
    where
        boxqs = boxAtomicQuestions qla qlb
        n = length boxqs
        packed = packList $ boxqs
        rel = transitiveClosure $ relationFromFunction n $ packRel packed (boxPrec (qla, qlb))


boxAtomicProduct :: (POrd a, POrd b) => QLogic a -> QLogic b -> QLogic (BoxProduct a b)
boxAtomicProduct qla qlb = boxLogic
        where
            boxLogic = QLogic boxElems boxLess boxOrtho
            boxElems = quotientBy (isEquivIn preql) els
            boxLess = liftRel isLess
            boxOrtho a = fromJust $ find (isOrtho a) boxElems 
            isOrtho a b = case boxPlus boxLogic a b of
                              Nothing -> False
                              Just c -> isEquivIn boxLogic c $ boxOne 
            boxOne = one boxLogic

            preql@(QLogic els isLess _) = boxAtomicProduct' qla qlb

boxAtomicProduct' :: (POrd a, POrd b) => QLogic a -> QLogic b -> QLogic (FreeProduct a b)
boxAtomicProduct' qla qlb = QLogic els isLess id
    where
        (packed, rel) = boxAtomicProductRel qla qlb
        els = packValues packed 
        isLess = unpackRel packed (relLess rel) 

boxPlus :: (POrd a, POrd b) => QLogic (BoxProduct a b) -> BoxProduct a b -> BoxProduct a b -> Maybe (BoxProduct a b)
boxPlus ql a b = equivLookup (elements ql) $ (equivRepr a) <+> (equivRepr b)

data Equiv a where
       Equiv :: (POrd a) => [a] -> Equiv a

instance (Show a) => Show (Equiv a) where
        show a = "[[" ++ (show $ equivRepr a) ++ "]]"

instance (Eq a) => Eq (Equiv a) where
        (Equiv a) == (Equiv b) = any (`elem` b) a

instance (Ord a) => Ord (Equiv a) where
        a `compare` b = equivRepr a `compare` equivRepr b

liftRel :: (a -> a -> Bool) -> Equiv a -> Equiv a -> Bool
liftRel rel a b = equivRepr a `rel` equivRepr b

equivRepr :: Equiv a -> a
equivRepr (Equiv e) = head . sort $ e

equivLookup :: [Equiv a] -> a -> Maybe (Equiv a)
equivLookup [] _ = Nothing
equivLookup (e@(Equiv ec):es) a
    | a `elem` ec = Just e
    | otherwise = equivLookup es a

quotientBy :: (POrd a) => (a -> a -> Bool) -> [a] -> [Equiv a]
quotientBy eq set = quotientBy' eq [] set

quotientBy' _ accum [] = accum
quotientBy' eq accum as@(a:_) = quotientBy' eq ((Equiv equal):accum) rest
    where
        (equal, rest) = partition (eq a) as


-- Examples
--
bwoAtomic = boxAtomicProduct lanternLogic lanternLogic
bwo = boxProduct lanternLogic lanternLogic
(packBWO, relBWO) = boxAtomicProductRel lanternLogic lanternLogic

