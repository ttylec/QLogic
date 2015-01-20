
module Data.Poset.Examples (Lantern(..)
                           , lanternPoset
                           , booleanPoset)
                           where

import Data.Poset
import Data.Relation
import Data.QLogic.Utils
import Data.QLogic.IO

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

-- |Boolean Poset (subsets of sample space)
booleanPoset :: (Eq a) => [a] -> Poset [a]
booleanPoset space = Poset elems (Function isSubset) 
    where
        elems = subsets space
        isSubset a b = all (`elem` b) a
