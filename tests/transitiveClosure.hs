module Main where

import Data.Poset
import Data.Relation

data Chain = Zero | A | B | C | D | One deriving (Bounded, Enum, Eq, Ord, Show)

instance POrd Chain where
        Zero .<=. Zero = True
        One .<=. One = True
        A .<=. A = True
        B .<=. B = True
        C .<=. C = True
        D .<=. D = True
        Zero .<=. A = True
        Zero .<=. B = True
        A .<=. C = True
        B .<=. D = True
        C .<=. One = True
        D .<=. One = True
        _ .<=. _ = False

chainPoset = fromPOrd [minBound..maxBound] :: Poset Chain
