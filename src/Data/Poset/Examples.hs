-- {-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Poset.Examples (Lantern(..)
                           , lanternPoset
                           , Subset(..)
                           , booleanPoset)
                           where

import Data.Poset
import Data.Relation
import Data.QLogic.Utils
import Data.QLogic.IO

import Data.Set (Set, fromList, isSubsetOf, difference, union)

import qualified Data.IntSet as S

-- = Examples
-- |Chinesse lantern POrd data (simple orthomodular lattice):
-- >     One
-- >   / | | \
-- > X0 X1 Y0 Y1 
-- >   \ | | /
-- >    Zero
data Lantern = Zero | X0 | X1 | Y0 | Y1 | One deriving (Bounded, Eq, Enum, Ord, Show)

instance Repr Lantern where
        repr = show

instance POrd Lantern where
        Zero .<=. _ = True
        _ .<=. One = True
        a .<=. b = a == b

-- |Chinesse lantern Poset
lanternPoset :: Poset Lantern
lanternPoset = fromPOrd [minBound..maxBound]

-- -- |Boolean Poset (subsets of sample space)
-- booleanPoset :: (Ord a) => [a] -> Poset (Set a)
-- booleanPoset space = Poset elems (Function isSubsetOf)
--     where
--         elems = map fromList $ subsets space
-- 
-- 
data Subset a = Subset (Set a) (Set a) deriving (Eq, Ord, Show)

booleanPoset :: (Ord a) => [a] -> Poset (Subset a)
booleanPoset space = Poset elems (Function subsetOf)
    where
        elems = map (Subset spaceSet . fromList) $ subsets space
        (Subset _ a) `subsetOf` (Subset _ b) = a `isSubsetOf` b
        spaceSet = fromList space 
-- 
-- instance (Ord a) => POrdStruct (Poset (Subset a)) (Subset a) where
--     elementsOf (Poset els _) = els
--     lessIn _ (Subset _ a) (Subset _ b) = a `isSubsetOf` b 
--     supIn poset (Subset space a) (Subset _ b) = Just $ Subset space $ union a b
