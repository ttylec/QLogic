{-# LANGUAGE GADTs, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module QLogic.Poset.Generic ( Poset(Poset)
                          , fromFunc, fromPOrd
                          , packPoset, packPoset', unpackPoset, sparsePoset
                          , fromASRelation, quotientPoset
                          , fromFuncM ) where
import Control.Monad

import QLogic.Utils
import QLogic.Poset
import QLogic.Relation

-- | Generic partially ordered set type.
data Poset a where
        Poset :: [a] -> Relation a -> Poset a

-- | 'Poset' is an instance of 'POrdStruct'.
instance POrdStruct (Poset a) a where
    elementsOf (Poset els _) = els
    lessIn (Poset _ rel) = inRelation rel

-- | Constructs Poset from list of elements and relation given by function
fromFunc :: [a] -> (a -> a -> Bool) -> Poset a
fromFunc els f = Poset els (Function f)

-- | Constructs Poset from monadic order function
fromFuncM :: (Ord a, Monad m) => [a] -> (a -> a -> m Bool) -> m (Poset a)
fromFuncM els f = liftM (Poset els) $ relationFromFuncM els f

-- | Constructs Poset from the list of 'POrd' data
fromPOrd :: POrd a => [a] -> Poset a
fromPOrd els = Poset els (Function (.<=.))

-- | Constructs Poset from arbitrary 'POrdStruct' data
fromPOrdStruct :: POrdStruct p a => p -> Poset a
fromPOrdStruct poset = Poset (elementsOf poset) $ Function (lessIn poset)

-- | Converts Poset relation to list representation
sparsePoset :: Ord a => Poset a -> Poset a
sparsePoset (Poset els rel) = Poset els $ sparseRelation els rel

-- | Convert Poset to packed representation: elements are replaced
-- by sequence of integers and relation is coverted to array representation.
packPoset :: Ord a => Poset a -> Poset Int
packPoset = snd . packPoset'

-- | Like 'packPoset' by returns also the 'Packed' type
-- to converent between packed and explicit representation.
packPoset' :: Ord a => Poset a -> (Packed a, Poset Int)
packPoset' (Poset els rel) = (packed, Poset [0..n-1] prel)
    where
        n = length els
        packed = packList els
        prel = packRelation packed rel

-- | Convert packed Poset representation to explicit one.
-- It is not safe: packed must have appropriate length (and it is not checked)
unpackPoset :: Ord a => Packed a -> Poset Int -> Poset a
unpackPoset packed (Poset els rel) = Poset (packedElements packed) (Function isLess)
    where
        isLess = unpackFunc2 packed (inRelation rel)

-- | Takes a set with anti-symmetric, reflexive relation 
-- and creates preposet by taking transitive closure of the relation.
fromASRelation :: Ord a => Poset a -> Poset a
fromASRelation set@(Poset els rel@(Function _)) = unpackPoset packed preposet
        where
            (packed, (Poset pels pr)) = packPoset' set
            preposet = Poset pels $ transitiveClosure pels pr
fromASRelation (Poset els rel) = Poset els $ transitiveClosure els rel

-- | Preorder is a partial order relation that is anti-symmetric
-- and transitive but not reflexive. Having a set with a preorder
-- we can construct partialy ordered set by taking the quotient:
quotientPoset :: (Ord a, POrdStruct p a) => p -> Poset (Equiv a)
quotientPoset preposet = Poset els equivLess
  where
      els = quotientBy (equalIn preposet) $ elementsOf preposet
      equivLess = Function $ liftFunc2 $ lessIn preposet

