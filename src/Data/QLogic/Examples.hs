
module Data.QLogic.Examples (Lantern(..)
                            , module Data.Poset.Examples
                            , lanternLogic
                            , booleanLogic
                            , concreteLogic)
                            where

import Data.QLogic
import Data.Poset.Examples
import Data.QLogic.Utils

import Data.Set (Set, fromList, isSubsetOf, difference)

-- = Examples

-- |Chinesse lantern logic
lanternLogic:: QLogic Lantern
lanternLogic = fromPoset lanternPoset lanternOcmpl
    where
        lanternOcmpl Zero = One
        lanternOcmpl X0 = X1
        lanternOcmpl X1 = X0
        lanternOcmpl Y0 = Y1
        lanternOcmpl Y1 = Y0
        lanternOcmpl One = Zero

-- |Boolean logic (subsets of sample space)
booleanLogic :: (Ord a) => [a] -> QLogic (Subset a)
booleanLogic space = fromPoset (booleanPoset space) booleanOcmpl
    where
        booleanOcmpl (Subset space a) = Subset space $ difference space a


concreteLogic :: (Ord a) => [a] -> [[a]] -> QLogic (Subset a)
concreteLogic space els = fromPoset (fromFunc elems subsetOf) booleanOcmpl 
    where
        elems = map (Subset spaceSet . fromList) $ subsets space
        (Subset _ a) `subsetOf` (Subset _ b) = a `isSubsetOf` b
        spaceSet = fromList space 
        booleanOcmpl (Subset space a) = Subset space $ difference space a


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
