-- {-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Poset.Examples (Lantern(..)
                           , lanternPoset
                           , Boolean3(..)
                           , boolean3Poset
                           , booleanPoset)
                           where

import Data.Poset
import Data.Relation
import Data.QLogic.Utils
import Data.QLogic.IO

import Data.IntSet (IntSet, fromList, isSubsetOf, difference, union)


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

data Boolean3 = Empty | S0 | S1 | S2 | S01 | S02 | S12 | S123 deriving (Bounded, Eq, Enum, Ord, Show)

instance POrd Boolean3 where
    Empty .<=. _ = True
    _  .<=. S123 = True
    S0 .<=. S01  = True
    S0 .<=. S02  = True
    S1 .<=. S01  = True
    S1 .<=. S12  = True 
    S2 .<=. S02  = True
    S2 .<=. S12  = True
    a  .<=. b    = a == b

boolean3Poset :: Poset Boolean3
boolean3Poset = fromPOrd [minBound..maxBound]    

-- |Boolean Poset (subsets of sample space)
-- booleanPoset :: (Ord a) => [a] -> Poset (Set a)
-- booleanPoset space = Poset elems (Function isSubsetOf)
--     where
--         elems = {-# SCC elems #-} map fromList $ subsets space

booleanPoset :: [Int] -> ConcretePoset
booleanPoset space = ConcretePoset elems
    where
        elems = {-# SCC elems #-} map fromList $ subsets space

-- 
-- instance (Ord a) => POrdStruct (Poset (Subset a)) (Subset a) where
--     elementsOf (Poset els _) = els
--     lessIn _ (Subset _ a) (Subset _ b) = a `isSubsetOf` b 
--     supIn poset (Subset space a) (Subset _ b) = Just $ Subset space $ union a b
