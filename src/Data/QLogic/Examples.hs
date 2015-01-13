
module Data.QLogic.Examples (Lantern(..)
                           , lanternLogic
                           , booleanLogic)
                           where

import Data.QLogic
import Data.Poset.Examples

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
booleanLogic :: (Eq a) => [a] -> QLogic [a]
booleanLogic space = fromPoset (booleanPoset space) booleanOcmpl
    where
        booleanOcmpl a = filter (not . (`elem` a)) space
