module Data.QLogic.Utils (implies, subsets)
    where

-- = General utility functions

-- |Logical implication
implies :: Bool -> Bool -> Bool
implies False False = True
implies False True = True
implies True False = False
implies True True = True

-- |Subsets of given list (uniqueness is not checked)
subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)
