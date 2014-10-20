module QLogic.Examples.Lattice8 where

import QLogic

-- |Lattice of three two-valued questions
data Lattice8 = Zero | X0 | X1 | Y0 | Y1 | Z0 | Z1 | One deriving (Bounded, Enum, Eq, Ord, Show)

instance Repr Lattice8 where
        repr = show

instance Finite Lattice8 where
        elements = [minBound..]

instance Poset Lattice8 where
        Zero .<. _ = True

        X1 .<. One = True
        X1 .<. X1 = True

        X0 .<. One = True 
        X0 .<. X0 = True

        Y1 .<. One = True
        Y1 .<. Y1 = True

        Y0 .<. One = True
        Y0 .<. Y0 = True

        Z0 .<. One = True
        Z0 .<. Z0 = True

        Z1 .<. One = True
        Z1 .<. Z1 = True

        One .<. One = True
        _ .<. _ = False

instance Logic Lattice8 where
        one = One
        zero = Zero
        ortho Zero = One
        ortho X1 = X0
        ortho X0 = X1
        ortho Y1 = Y0
        ortho Y0 = Y1
        ortho Z0 = Z1
        ortho Z1 = Z0
        ortho One = Zero

instance AtomicLogic Lattice8 where
        atoms = [X1, X0, Y1, Y0, Z0, Z1]
