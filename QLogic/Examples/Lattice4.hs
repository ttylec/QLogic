module QLogic.Examples.Lattice4 where

import QLogic

-- |Simple four-atom lattice
data Lattice4 = Zero | X1 | X0 | Y1 | Y0 | One deriving (Bounded, Enum, Eq, Ord, Show)

instance Repr Lattice4 where
        repr = show

instance Finite Lattice4 where
        elements = [minBound..]

instance Poset Lattice4 where
        Zero .<. _ = True

        X1 .<. One = True
        X1 .<. X1 = True

        X0 .<. One = True 
        X0 .<. X0 = True

        Y1 .<. One = True
        Y1 .<. Y1 = True

        Y0 .<. One = True
        Y0 .<. Y0 = True

        One .<. One = True
        _ .<. _ = False

instance Logic Lattice4 where
        one = One
        zero = Zero
        ortho Zero = One
        ortho X1 = X0
        ortho X0 = X1
        ortho Y1 = Y0
        ortho Y0 = Y1
        ortho One = Zero

instance AtomicLogic Lattice4 where
        atoms = [X1, X0, Y1, Y0]

