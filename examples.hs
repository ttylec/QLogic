module QLogic.Examples where

import QLogic
import Data.List

-- |Simple example of quantum logic 
data SimpleElements = Zero | A | B | One deriving (Enum, Bounded, Show, Eq)
simpleRelation :: SimpleElements -> SimpleElements -> Bool
simpleRelation p q
    | p == q = True
simpleRelation Zero _ = True
simpleRelation _ Zero = False
simpleRelation One _ = False
simpleRelation _ One = True
simpleRelation _ _ = False

instance Finite SimpleElements where
        elements = [minBound..]

instance Poset SimpleElements where
        (.<.) = simpleRelation

instance Logic SimpleElements where
        one = One
        zero = Zero
        ortho Zero = One
        ortho One = Zero
        ortho A = B
        ortho B = A

-- |Logic of subsets of 4 element set
newtype Space4 = Space4 [Int] deriving (Show)

instance Eq Space4 where
        (Space4 a) == (Space4 b) = (sort a) == (sort b)

instance Finite Space4 where
        elements = map Space4 $ subsets [1, 2, 3, 4] 

instance Poset Space4 where
       (.<.) (Space4 a) (Space4 b) = all (`elem` b) a

instance Logic Space4 where
        one = Space4 [1, 2, 3, 4]
        zero = Space4 []
        ortho (Space4 a) = Space4 $ [1, 2, 3, 4] \\ a
        (Space4 a) /\ (Space4 b) = Just $ Space4 $ intersect a b
        (Space4 a) \/ (Space4 b) = Just $ Space4 $ union a b
