module Data.QLogic.Examples 
where

import Data.QLogic

booleanLogic :: Int -> QLogic [Int]
booleanLogic n = fromList elems isSubset complement
    where
        space = [1..n]
        elems = subsets space
        isSubset a b = all (`elem` b) a
        complement a = filter (not . (`elem` a)) space

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

data Boolean3 = BEmpty | B1 | B2 | B3 | B12 | B13 | B23 | BOne deriving (Bounded, Eq, Enum, Ord, Show)

instance POrd Boolean3 where
        BEmpty ≤ _ = True
        _ ≤ BOne = True
        B1 ≤ B12 = True
        B1 ≤ B13 = True
        B2 ≤ B12 = True
        B2 ≤ B23 = True
        B3 ≤ B13 = True
        B3 ≤ B23 = True
        _ ≤ _ = False

boolean3Logic = fromPOrd boolean3Elements boolean3Ortho

boolean3Elements :: [Boolean3]
boolean3Elements = [minBound..maxBound]

boolean3Ortho :: Boolean3 -> Boolean3
boolean3Ortho BEmpty = BOne
boolean3Ortho BOne   = BEmpty
boolean3Ortho B1     = B23
boolean3Ortho B2     = B13
boolean3Ortho B3     = B12
boolean3Ortho B12    = B3 
boolean3Ortho B13    = B2 
boolean3Ortho B23    = B1 

data Lantern = Zero | X0 | X1 | Y0 | Y1 | One deriving (Bounded, Eq, Enum, Ord, Show)

instance POrd Lantern where
        Zero ≤ _ = True
        _ ≤ One = True
        a ≤ b = a == b

lanternLogic = fromPOrd lanternElements lanternOrtho

lanternElements :: [Lantern]
lanternElements = [minBound..maxBound]

lanternOrtho :: Lantern -> Lantern
lanternOrtho Zero = One
lanternOrtho One  = Zero
lanternOrtho X0   = X1
lanternOrtho X1   = X0
lanternOrtho Y0   = Y1
lanternOrtho Y1   = Y0
