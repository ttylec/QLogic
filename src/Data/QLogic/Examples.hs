
module Data.QLogic.Examples (Lantern(..)
                            , module Data.Poset.Examples
                            , lanternLogic
                            , boolean3Logic
                            , booleanLogic
                            , concreteLogic
                            , evenSubsets
                            , threeBoxLogic)
                            where

import Data.QLogic
import Data.Poset.Examples
import Data.QLogic.Utils

import Data.IntSet (IntSet, fromList, isSubsetOf, difference)

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
booleanLogic :: [Int] -> QLogic ConcretePoset IntSet
booleanLogic space = fromPoset (booleanPoset space) booleanOcmpl
    where
        spaceSet = fromList space
        booleanOcmpl = difference spaceSet


concreteLogic :: [Int] -> [[Int]] -> QLogic ConcretePoset IntSet
concreteLogic space els = fromPoset (ConcretePoset elems) booleanOcmpl 
    where
        elems = map fromList els
        spaceSet = fromList space
        booleanOcmpl = difference spaceSet


evenSubsets :: Int -> QLogic ConcretePoset IntSet
evenSubsets n = concreteLogic space $ filter (even . length) $ subsets space
    where
        space = [0..n-1]

data ThreeBox = TZero | A0 | A1 | B0 | B1 | C0 | C1 | A0C0 | A1C0 | B0C0 | B1C0 | TOne deriving (Bounded, Eq, Enum, Ord, Show)

instance POrd ThreeBox where
    TZero .<=. _ = True
    _ .<=. TOne  = True
    A0 .<=. C1   = True
    A0 .<=. A0C0 = True
    A1 .<=. C1   = True
    A1 .<=. A1C0 = True
    B0 .<=. C1   = True
    B0 .<=. B0C0 = True
    B1 .<=. C1   = True
    B1 .<=. B1C0 = True
    C0 .<=. A0C0 = True
    C0 .<=. A1C0 = True
    C0 .<=. B0C0 = True
    C0 .<=. B1C0 = True
    a .<=. b     = a == b

threeBoxLogic :: QLogic (Poset ThreeBox) ThreeBox
threeBoxLogic = fromPoset (fromPOrd els) ortho
    where
        els = [minBound..maxBound] :: [ThreeBox]
        ortho TZero = TOne
        ortho TOne  = TZero
        ortho A0 = A1C0
        ortho A1 = A0C0
        ortho B0 = B1C0
        ortho B1 = B0C0
        ortho C0 = C1
        ortho C1 = C0
        ortho A0C0 = A1
        ortho A1C0 = A0
        ortho B0C0 = B1
        ortho B1C0 = B0


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
