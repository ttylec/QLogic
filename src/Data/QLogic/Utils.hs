module Data.QLogic.Utils (implies, iff, subsets, isSubset)
    where

-- = General utility functions

-- |Logical implication
implies :: Bool -> Bool -> Bool
implies False False = True
implies False True = True
implies True False = False
implies True True = True

-- |Logical if and only if
iff :: Bool -> Bool -> Bool
iff False False = True
iff False True = False
iff True False = False
iff True True = True

-- |Subsets of given list (uniqueness is not checked)
subsets :: [a] -> [[a]]

subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)
        
isSubset :: Eq a => [a] -> [a] -> Bool
isSubset a b = all (`elem` b) a
