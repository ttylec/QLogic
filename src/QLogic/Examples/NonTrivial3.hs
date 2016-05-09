module QLogic.Examples.NonTrivial3 where

import QLogic
import QLogic.Poset.Generic

data Triple = Zero | X | Y | X' | Y' | Z | XY | XZ | YZ | X'Z | Y'Z | One
                    deriving (Bounded, Eq, Enum, Ord, Read, Show)

instance POrd Triple where
    Zero .<=. _   = True
    _    .<=. One = True

    X .<=. XY = True
    X .<=. XZ = True
    Y .<=. XY = True
    Y .<=. YZ = True

    X' .<=. XY = True
    X' .<=. X'Z = True
    Y' .<=. XY = True
    Y' .<=. Y'Z = True

    Z .<=. XZ  = True
    Z .<=. YZ  = True
    Z .<=. X'Z = True
    Z .<=. Y'Z = True

    a .<=. b = a == b

tripleLogic :: QLogic (Poset Triple) Triple
tripleLogic = fromPOrdStruct (fromPOrd els) ortho
    where
        els = [minBound..maxBound] :: [Triple]
        ortho Zero = One
        ortho X = YZ
        ortho Y = XZ
        ortho X' = Y'Z
        ortho Y' = X'Z
        ortho Z = XY
        ortho XY = Z
        ortho XZ = Y
        ortho YZ = X
        ortho X'Z = Y'
        ortho Y'Z = X'
        ortho One = Zero
