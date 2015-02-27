
module Data.QLogic.Examples (Lantern(..)
                            , module Data.Poset.Examples
                            , lanternLogic
                            , boolean3Logic
                            , booleanLogic
                            , concreteLogic)
                            where

import Data.QLogic
import Data.Poset.Examples
import Data.QLogic.Utils

import Data.Set (Set, fromList, isSubsetOf, difference)

-- = Examples

-- |Chinesse lantern logic
lanternLogic :: QLogic (Poset Lantern) Lantern
lanternLogic = fromPoset lanternPoset lanternOcmpl
    where
        lanternOcmpl Zero = One
        lanternOcmpl X0 = X1
        lanternOcmpl X1 = X0
        lanternOcmpl Y0 = Y1
        lanternOcmpl Y1 = Y0
        lanternOcmpl One = Zero

boolean3Logic :: QLogic (Poset Boolean3) Boolean3
boolean3Logic = fromPoset boolean3Poset b3ortho
    where
        b3ortho Empty = S123
        b3ortho S0    = S12
        b3ortho S1    = S02
        b3ortho S2    = S01
        b3ortho S01   = S2
        b3ortho S02   = S1
        b3ortho S12   = S0
        b3ortho S123  = Empty

-- |Boolean logic (subsets of sample space)
-- booleanLogic :: (Ord a) => [a] -> QLogic (ConcretePoset a) (Set a)
booleanLogic space = fromPoset (booleanPoset space) booleanOcmpl
    where
        spaceSet = fromList space
        booleanOcmpl = difference spaceSet


concreteLogic :: (Ord a) => [a] -> [[a]] -> QLogic (ConcretePoset a) (Set a)
concreteLogic space els = fromPoset (ConcretePoset elems) booleanOcmpl 
    where
        elems = map fromList $ subsets space
        spaceSet = fromList space
        booleanOcmpl = difference spaceSet


-- this is not a logic (sup law does not hold)
-- data Envelope = EZero | L0 | R0 | L1 | R1 | EOne deriving (Bounded, Eq, Enum, Ord, Show)
-- 
-- instance POrd Envelope where
--         EZero .<=. _ = True
--         _ .<=. EOne = True
--         L0 .<=. L1 = True
--         L0 .<=. R1 = True
--         R0 .<=. L1 = True
--         R0 .<=. R1 = True
--         _ .<=. _ = False
-- 
-- envelopeLogic :: QLogic Envelope
-- envelopeLogic = fromPoset (fromPOrd els) ortho
--     where
--         els = [minBound..maxBound] :: [Envelope]
--         ortho EZero = EOne
--         ortho EOne = EZero
--         ortho L0 = R1
--         ortho R0 = L1
--         ortho L1 = R0
--         ortho R1 = L0
