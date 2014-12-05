module Data.QLogic.Examples.TwoTwoBoxWorld (twoTwoBoxWorldLogic) 
where

import Data.QLogic
data TwoTwoBoxWorld = ZeroZero | OneOne | X1X1 | X1X0 | X1Y1 | X1Y0 | X0X1 | X0X0 | X0Y1 | X0Y0 | Y1X1 | Y1X0 | Y1Y1 | Y1Y0 | Y0X1 | Y0X0 | Y0Y1 | Y0Y0 | X1X1plusX1X0 | X1X1plusX0X1 | X1X1plusX0X0 | X1X1plusX0Y1 | X1X1plusX0Y0 | X1X1plusY1X0 | X1X1plusY0X0 | X1X0plusX0X1 | X1X0plusX0X0 | X1X0plusX0Y1 | X1X0plusX0Y0 | X1X0plusY1X1 | X1X0plusY0X1 | X1Y1plusX0X1 | X1Y1plusX0X0 | X1Y1plusX0Y1 | X1Y1plusX0Y0 | X1Y1plusY1Y0 | X1Y1plusY0Y0 | X1Y0plusX0X1 | X1Y0plusX0X0 | X1Y0plusX0Y1 | X1Y0plusX0Y0 | X1Y0plusY1Y1 | X1Y0plusY0Y1 | X0X1plusX0X0 | X0X1plusY1X0 | X0X1plusY0X0 | X0X0plusY1X1 | X0X0plusY0X1 | X0Y1plusY1Y0 | X0Y1plusY0Y0 | X0Y0plusY1Y1 | X0Y0plusY0Y1 | Y1X1plusY1X0 | Y1X1plusY0X0 | Y1X1plusY0Y1 | Y1X1plusY0Y0 | Y1X0plusY0X1 | Y1X0plusY0Y1 | Y1X0plusY0Y0 | Y1Y1plusY0X1 | Y1Y1plusY0X0 | Y1Y1plusY0Y0 | Y1Y0plusY0X1 | Y1Y0plusY0X0 | Y1Y0plusY0Y1 | Y0X1plusY0X0 | X1X1plusX1X0plusX0X1 | X1X1plusX1X0plusX0X0 | X1X1plusX1X0plusX0Y1 | X1X1plusX1X0plusX0Y0 | X1X1plusX0X1plusX0X0 | X1X1plusX0X1plusY1X0 | X1X1plusX0X1plusY0X0 | X1X0plusX0X1plusX0X0 | X1X0plusX0X0plusY1X1 | X1X0plusX0X0plusY0X1 | X1Y1plusX0X1plusX0X0 | X1Y1plusX0Y1plusY1Y0 | X1Y1plusX0Y1plusY0Y0 | X1Y0plusX0X1plusX0X0 | X1Y0plusX0Y0plusY1Y1 | X1Y0plusX0Y0plusY0Y1 deriving (Enum, Bounded, Eq, Ord, Show)

twoTwoBoxWorldLogic = fromPOrd [minBound..maxBound] twoTwoBoxOrtho

instance POrd TwoTwoBoxWorld where
    ZeroZero ≤ ZeroZero = True
    ZeroZero ≤ OneOne = True
    ZeroZero ≤ X1X1 = True
    ZeroZero ≤ X1X0 = True
    ZeroZero ≤ X1Y1 = True
    ZeroZero ≤ X1Y0 = True
    ZeroZero ≤ X0X1 = True
    ZeroZero ≤ X0X0 = True
    ZeroZero ≤ X0Y1 = True
    ZeroZero ≤ X0Y0 = True
    ZeroZero ≤ Y1X1 = True
    ZeroZero ≤ Y1X0 = True
    ZeroZero ≤ Y1Y1 = True
    ZeroZero ≤ Y1Y0 = True
    ZeroZero ≤ Y0X1 = True
    ZeroZero ≤ Y0X0 = True
    ZeroZero ≤ Y0Y1 = True
    ZeroZero ≤ Y0Y0 = True
    ZeroZero ≤ X1X1plusX1X0 = True
    ZeroZero ≤ X1X1plusX0X1 = True
    ZeroZero ≤ X1X1plusX0X0 = True
    ZeroZero ≤ X1X1plusX0Y1 = True
    ZeroZero ≤ X1X1plusX0Y0 = True
    ZeroZero ≤ X1X1plusY1X0 = True
    ZeroZero ≤ X1X1plusY0X0 = True
    ZeroZero ≤ X1X0plusX0X1 = True
    ZeroZero ≤ X1X0plusX0X0 = True
    ZeroZero ≤ X1X0plusX0Y1 = True
    ZeroZero ≤ X1X0plusX0Y0 = True
    ZeroZero ≤ X1X0plusY1X1 = True
    ZeroZero ≤ X1X0plusY0X1 = True
    ZeroZero ≤ X1Y1plusX0X1 = True
    ZeroZero ≤ X1Y1plusX0X0 = True
    ZeroZero ≤ X1Y1plusX0Y1 = True
    ZeroZero ≤ X1Y1plusX0Y0 = True
    ZeroZero ≤ X1Y1plusY1Y0 = True
    ZeroZero ≤ X1Y1plusY0Y0 = True
    ZeroZero ≤ X1Y0plusX0X1 = True
    ZeroZero ≤ X1Y0plusX0X0 = True
    ZeroZero ≤ X1Y0plusX0Y1 = True
    ZeroZero ≤ X1Y0plusX0Y0 = True
    ZeroZero ≤ X1Y0plusY1Y1 = True
    ZeroZero ≤ X1Y0plusY0Y1 = True
    ZeroZero ≤ X0X1plusX0X0 = True
    ZeroZero ≤ X0X1plusY1X0 = True
    ZeroZero ≤ X0X1plusY0X0 = True
    ZeroZero ≤ X0X0plusY1X1 = True
    ZeroZero ≤ X0X0plusY0X1 = True
    ZeroZero ≤ X0Y1plusY1Y0 = True
    ZeroZero ≤ X0Y1plusY0Y0 = True
    ZeroZero ≤ X0Y0plusY1Y1 = True
    ZeroZero ≤ X0Y0plusY0Y1 = True
    ZeroZero ≤ Y1X1plusY1X0 = True
    ZeroZero ≤ Y1X1plusY0X0 = True
    ZeroZero ≤ Y1X1plusY0Y1 = True
    ZeroZero ≤ Y1X1plusY0Y0 = True
    ZeroZero ≤ Y1X0plusY0X1 = True
    ZeroZero ≤ Y1X0plusY0Y1 = True
    ZeroZero ≤ Y1X0plusY0Y0 = True
    ZeroZero ≤ Y1Y1plusY0X1 = True
    ZeroZero ≤ Y1Y1plusY0X0 = True
    ZeroZero ≤ Y1Y1plusY0Y0 = True
    ZeroZero ≤ Y1Y0plusY0X1 = True
    ZeroZero ≤ Y1Y0plusY0X0 = True
    ZeroZero ≤ Y1Y0plusY0Y1 = True
    ZeroZero ≤ Y0X1plusY0X0 = True
    ZeroZero ≤ X1X1plusX1X0plusX0X1 = True
    ZeroZero ≤ X1X1plusX1X0plusX0X0 = True
    ZeroZero ≤ X1X1plusX1X0plusX0Y1 = True
    ZeroZero ≤ X1X1plusX1X0plusX0Y0 = True
    ZeroZero ≤ X1X1plusX0X1plusX0X0 = True
    ZeroZero ≤ X1X1plusX0X1plusY1X0 = True
    ZeroZero ≤ X1X1plusX0X1plusY0X0 = True
    ZeroZero ≤ X1X0plusX0X1plusX0X0 = True
    ZeroZero ≤ X1X0plusX0X0plusY1X1 = True
    ZeroZero ≤ X1X0plusX0X0plusY0X1 = True
    ZeroZero ≤ X1Y1plusX0X1plusX0X0 = True
    ZeroZero ≤ X1Y1plusX0Y1plusY1Y0 = True
    ZeroZero ≤ X1Y1plusX0Y1plusY0Y0 = True
    ZeroZero ≤ X1Y0plusX0X1plusX0X0 = True
    ZeroZero ≤ X1Y0plusX0Y0plusY1Y1 = True
    ZeroZero ≤ X1Y0plusX0Y0plusY0Y1 = True

    OneOne ≤ OneOne = True

    X1X1 ≤ OneOne = True
    X1X1 ≤ X1X1 = True
    X1X1 ≤ X1X1plusX1X0 = True
    X1X1 ≤ X1X1plusX0X1 = True
    X1X1 ≤ X1X1plusX0X0 = True
    X1X1 ≤ X1X1plusX0Y1 = True
    X1X1 ≤ X1X1plusX0Y0 = True
    X1X1 ≤ X1X1plusY1X0 = True
    X1X1 ≤ X1X1plusY0X0 = True
    X1X1 ≤ X1X1plusX1X0plusX0X1 = True
    X1X1 ≤ X1X1plusX1X0plusX0X0 = True
    X1X1 ≤ X1X1plusX1X0plusX0Y1 = True
    X1X1 ≤ X1X1plusX1X0plusX0Y0 = True
    X1X1 ≤ X1X1plusX0X1plusX0X0 = True
    X1X1 ≤ X1X1plusX0X1plusY1X0 = True
    X1X1 ≤ X1X1plusX0X1plusY0X0 = True

    X1X0 ≤ OneOne = True
    X1X0 ≤ X1X0 = True
    X1X0 ≤ X1X1plusX1X0 = True
    X1X0 ≤ X1X0plusX0X1 = True
    X1X0 ≤ X1X0plusX0X0 = True
    X1X0 ≤ X1X0plusX0Y1 = True
    X1X0 ≤ X1X0plusX0Y0 = True
    X1X0 ≤ X1X0plusY1X1 = True
    X1X0 ≤ X1X0plusY0X1 = True
    X1X0 ≤ X1X1plusX1X0plusX0X1 = True
    X1X0 ≤ X1X1plusX1X0plusX0X0 = True
    X1X0 ≤ X1X1plusX1X0plusX0Y1 = True
    X1X0 ≤ X1X1plusX1X0plusX0Y0 = True
    X1X0 ≤ X1X0plusX0X1plusX0X0 = True
    X1X0 ≤ X1X0plusX0X0plusY1X1 = True
    X1X0 ≤ X1X0plusX0X0plusY0X1 = True

    X1Y1 ≤ OneOne = True
    X1Y1 ≤ X1Y1 = True
    X1Y1 ≤ X1X1plusX1X0 = True
    X1Y1 ≤ X1Y1plusX0X1 = True
    X1Y1 ≤ X1Y1plusX0X0 = True
    X1Y1 ≤ X1Y1plusX0Y1 = True
    X1Y1 ≤ X1Y1plusX0Y0 = True
    X1Y1 ≤ X1Y1plusY1Y0 = True
    X1Y1 ≤ X1Y1plusY0Y0 = True
    X1Y1 ≤ X1X1plusX1X0plusX0X1 = True
    X1Y1 ≤ X1X1plusX1X0plusX0X0 = True
    X1Y1 ≤ X1X1plusX1X0plusX0Y1 = True
    X1Y1 ≤ X1X1plusX1X0plusX0Y0 = True
    X1Y1 ≤ X1Y1plusX0X1plusX0X0 = True
    X1Y1 ≤ X1Y1plusX0Y1plusY1Y0 = True
    X1Y1 ≤ X1Y1plusX0Y1plusY0Y0 = True

    X1Y0 ≤ OneOne = True
    X1Y0 ≤ X1Y0 = True
    X1Y0 ≤ X1X1plusX1X0 = True
    X1Y0 ≤ X1Y0plusX0X1 = True
    X1Y0 ≤ X1Y0plusX0X0 = True
    X1Y0 ≤ X1Y0plusX0Y1 = True
    X1Y0 ≤ X1Y0plusX0Y0 = True
    X1Y0 ≤ X1Y0plusY1Y1 = True
    X1Y0 ≤ X1Y0plusY0Y1 = True
    X1Y0 ≤ X1X1plusX1X0plusX0X1 = True
    X1Y0 ≤ X1X1plusX1X0plusX0X0 = True
    X1Y0 ≤ X1X1plusX1X0plusX0Y1 = True
    X1Y0 ≤ X1X1plusX1X0plusX0Y0 = True
    X1Y0 ≤ X1Y0plusX0X1plusX0X0 = True
    X1Y0 ≤ X1Y0plusX0Y0plusY1Y1 = True
    X1Y0 ≤ X1Y0plusX0Y0plusY0Y1 = True

    X0X1 ≤ OneOne = True
    X0X1 ≤ X0X1 = True
    X0X1 ≤ X1X1plusX0X1 = True
    X0X1 ≤ X1X0plusX0X1 = True
    X0X1 ≤ X1Y1plusX0X1 = True
    X0X1 ≤ X1Y0plusX0X1 = True
    X0X1 ≤ X0X1plusX0X0 = True
    X0X1 ≤ X0X1plusY1X0 = True
    X0X1 ≤ X0X1plusY0X0 = True
    X0X1 ≤ X1X1plusX1X0plusX0X1 = True
    X0X1 ≤ X1X1plusX0X1plusX0X0 = True
    X0X1 ≤ X1X1plusX0X1plusY1X0 = True
    X0X1 ≤ X1X1plusX0X1plusY0X0 = True
    X0X1 ≤ X1X0plusX0X1plusX0X0 = True
    X0X1 ≤ X1Y1plusX0X1plusX0X0 = True
    X0X1 ≤ X1Y0plusX0X1plusX0X0 = True

    X0X0 ≤ OneOne = True
    X0X0 ≤ X0X0 = True
    X0X0 ≤ X1X1plusX0X0 = True
    X0X0 ≤ X1X0plusX0X0 = True
    X0X0 ≤ X1Y1plusX0X0 = True
    X0X0 ≤ X1Y0plusX0X0 = True
    X0X0 ≤ X0X1plusX0X0 = True
    X0X0 ≤ X0X0plusY1X1 = True
    X0X0 ≤ X0X0plusY0X1 = True
    X0X0 ≤ X1X1plusX1X0plusX0X0 = True
    X0X0 ≤ X1X1plusX0X1plusX0X0 = True
    X0X0 ≤ X1X0plusX0X1plusX0X0 = True
    X0X0 ≤ X1X0plusX0X0plusY1X1 = True
    X0X0 ≤ X1X0plusX0X0plusY0X1 = True
    X0X0 ≤ X1Y1plusX0X1plusX0X0 = True
    X0X0 ≤ X1Y0plusX0X1plusX0X0 = True

    X0Y1 ≤ OneOne = True
    X0Y1 ≤ X0Y1 = True
    X0Y1 ≤ X1X1plusX0Y1 = True
    X0Y1 ≤ X1X0plusX0Y1 = True
    X0Y1 ≤ X1Y1plusX0Y1 = True
    X0Y1 ≤ X1Y0plusX0Y1 = True
    X0Y1 ≤ X0X1plusX0X0 = True
    X0Y1 ≤ X0Y1plusY1Y0 = True
    X0Y1 ≤ X0Y1plusY0Y0 = True
    X0Y1 ≤ X1X1plusX1X0plusX0Y1 = True
    X0Y1 ≤ X1X1plusX0X1plusX0X0 = True
    X0Y1 ≤ X1X0plusX0X1plusX0X0 = True
    X0Y1 ≤ X1Y1plusX0X1plusX0X0 = True
    X0Y1 ≤ X1Y1plusX0Y1plusY1Y0 = True
    X0Y1 ≤ X1Y1plusX0Y1plusY0Y0 = True
    X0Y1 ≤ X1Y0plusX0X1plusX0X0 = True

    X0Y0 ≤ OneOne = True
    X0Y0 ≤ X0Y0 = True
    X0Y0 ≤ X1X1plusX0Y0 = True
    X0Y0 ≤ X1X0plusX0Y0 = True
    X0Y0 ≤ X1Y1plusX0Y0 = True
    X0Y0 ≤ X1Y0plusX0Y0 = True
    X0Y0 ≤ X0X1plusX0X0 = True
    X0Y0 ≤ X0Y0plusY1Y1 = True
    X0Y0 ≤ X0Y0plusY0Y1 = True
    X0Y0 ≤ X1X1plusX1X0plusX0Y0 = True
    X0Y0 ≤ X1X1plusX0X1plusX0X0 = True
    X0Y0 ≤ X1X0plusX0X1plusX0X0 = True
    X0Y0 ≤ X1Y1plusX0X1plusX0X0 = True
    X0Y0 ≤ X1Y0plusX0X1plusX0X0 = True
    X0Y0 ≤ X1Y0plusX0Y0plusY1Y1 = True
    X0Y0 ≤ X1Y0plusX0Y0plusY0Y1 = True

    Y1X1 ≤ OneOne = True
    Y1X1 ≤ Y1X1 = True
    Y1X1 ≤ X1X1plusX0X1 = True
    Y1X1 ≤ X1X0plusY1X1 = True
    Y1X1 ≤ X0X0plusY1X1 = True
    Y1X1 ≤ Y1X1plusY1X0 = True
    Y1X1 ≤ Y1X1plusY0X0 = True
    Y1X1 ≤ Y1X1plusY0Y1 = True
    Y1X1 ≤ Y1X1plusY0Y0 = True
    Y1X1 ≤ X1X1plusX1X0plusX0X1 = True
    Y1X1 ≤ X1X1plusX0X1plusX0X0 = True
    Y1X1 ≤ X1X1plusX0X1plusY1X0 = True
    Y1X1 ≤ X1X1plusX0X1plusY0X0 = True
    Y1X1 ≤ X1X0plusX0X0plusY1X1 = True
    Y1X1 ≤ X1Y1plusX0Y1plusY1Y0 = True
    Y1X1 ≤ X1Y0plusX0Y0plusY1Y1 = True

    Y1X0 ≤ OneOne = True
    Y1X0 ≤ Y1X0 = True
    Y1X0 ≤ X1X1plusY1X0 = True
    Y1X0 ≤ X1X0plusX0X0 = True
    Y1X0 ≤ X0X1plusY1X0 = True
    Y1X0 ≤ Y1X1plusY1X0 = True
    Y1X0 ≤ Y1X0plusY0X1 = True
    Y1X0 ≤ Y1X0plusY0Y1 = True
    Y1X0 ≤ Y1X0plusY0Y0 = True
    Y1X0 ≤ X1X1plusX1X0plusX0X0 = True
    Y1X0 ≤ X1X1plusX0X1plusY1X0 = True
    Y1X0 ≤ X1X0plusX0X1plusX0X0 = True
    Y1X0 ≤ X1X0plusX0X0plusY1X1 = True
    Y1X0 ≤ X1X0plusX0X0plusY0X1 = True
    Y1X0 ≤ X1Y1plusX0Y1plusY1Y0 = True
    Y1X0 ≤ X1Y0plusX0Y0plusY1Y1 = True

    Y1Y1 ≤ OneOne = True
    Y1Y1 ≤ Y1Y1 = True
    Y1Y1 ≤ X1Y1plusX0Y1 = True
    Y1Y1 ≤ X1Y0plusY1Y1 = True
    Y1Y1 ≤ X0Y0plusY1Y1 = True
    Y1Y1 ≤ Y1X1plusY1X0 = True
    Y1Y1 ≤ Y1Y1plusY0X1 = True
    Y1Y1 ≤ Y1Y1plusY0X0 = True
    Y1Y1 ≤ Y1Y1plusY0Y0 = True
    Y1Y1 ≤ X1X1plusX1X0plusX0Y1 = True
    Y1Y1 ≤ X1X1plusX0X1plusY1X0 = True
    Y1Y1 ≤ X1X0plusX0X0plusY1X1 = True
    Y1Y1 ≤ X1Y1plusX0X1plusX0X0 = True
    Y1Y1 ≤ X1Y1plusX0Y1plusY1Y0 = True
    Y1Y1 ≤ X1Y1plusX0Y1plusY0Y0 = True
    Y1Y1 ≤ X1Y0plusX0Y0plusY1Y1 = True

    Y1Y0 ≤ OneOne = True
    Y1Y0 ≤ Y1Y0 = True
    Y1Y0 ≤ X1Y1plusY1Y0 = True
    Y1Y0 ≤ X1Y0plusX0Y0 = True
    Y1Y0 ≤ X0Y1plusY1Y0 = True
    Y1Y0 ≤ Y1X1plusY1X0 = True
    Y1Y0 ≤ Y1Y0plusY0X1 = True
    Y1Y0 ≤ Y1Y0plusY0X0 = True
    Y1Y0 ≤ Y1Y0plusY0Y1 = True
    Y1Y0 ≤ X1X1plusX1X0plusX0Y0 = True
    Y1Y0 ≤ X1X1plusX0X1plusY1X0 = True
    Y1Y0 ≤ X1X0plusX0X0plusY1X1 = True
    Y1Y0 ≤ X1Y1plusX0Y1plusY1Y0 = True
    Y1Y0 ≤ X1Y0plusX0X1plusX0X0 = True
    Y1Y0 ≤ X1Y0plusX0Y0plusY1Y1 = True
    Y1Y0 ≤ X1Y0plusX0Y0plusY0Y1 = True

    Y0X1 ≤ OneOne = True
    Y0X1 ≤ Y0X1 = True
    Y0X1 ≤ X1X1plusX0X1 = True
    Y0X1 ≤ X1X0plusY0X1 = True
    Y0X1 ≤ X0X0plusY0X1 = True
    Y0X1 ≤ Y1X0plusY0X1 = True
    Y0X1 ≤ Y1Y1plusY0X1 = True
    Y0X1 ≤ Y1Y0plusY0X1 = True
    Y0X1 ≤ Y0X1plusY0X0 = True
    Y0X1 ≤ X1X1plusX1X0plusX0X1 = True
    Y0X1 ≤ X1X1plusX0X1plusX0X0 = True
    Y0X1 ≤ X1X1plusX0X1plusY1X0 = True
    Y0X1 ≤ X1X1plusX0X1plusY0X0 = True
    Y0X1 ≤ X1X0plusX0X0plusY0X1 = True
    Y0X1 ≤ X1Y1plusX0Y1plusY0Y0 = True
    Y0X1 ≤ X1Y0plusX0Y0plusY0Y1 = True

    Y0X0 ≤ OneOne = True
    Y0X0 ≤ Y0X0 = True
    Y0X0 ≤ X1X1plusY0X0 = True
    Y0X0 ≤ X1X0plusX0X0 = True
    Y0X0 ≤ X0X1plusY0X0 = True
    Y0X0 ≤ Y1X1plusY0X0 = True
    Y0X0 ≤ Y1Y1plusY0X0 = True
    Y0X0 ≤ Y1Y0plusY0X0 = True
    Y0X0 ≤ Y0X1plusY0X0 = True
    Y0X0 ≤ X1X1plusX1X0plusX0X0 = True
    Y0X0 ≤ X1X1plusX0X1plusY0X0 = True
    Y0X0 ≤ X1X0plusX0X1plusX0X0 = True
    Y0X0 ≤ X1X0plusX0X0plusY1X1 = True
    Y0X0 ≤ X1X0plusX0X0plusY0X1 = True
    Y0X0 ≤ X1Y1plusX0Y1plusY0Y0 = True
    Y0X0 ≤ X1Y0plusX0Y0plusY0Y1 = True

    Y0Y1 ≤ OneOne = True
    Y0Y1 ≤ Y0Y1 = True
    Y0Y1 ≤ X1Y1plusX0Y1 = True
    Y0Y1 ≤ X1Y0plusY0Y1 = True
    Y0Y1 ≤ X0Y0plusY0Y1 = True
    Y0Y1 ≤ Y1X1plusY0Y1 = True
    Y0Y1 ≤ Y1X0plusY0Y1 = True
    Y0Y1 ≤ Y1Y0plusY0Y1 = True
    Y0Y1 ≤ Y0X1plusY0X0 = True
    Y0Y1 ≤ X1X1plusX1X0plusX0Y1 = True
    Y0Y1 ≤ X1X1plusX0X1plusY0X0 = True
    Y0Y1 ≤ X1X0plusX0X0plusY0X1 = True
    Y0Y1 ≤ X1Y1plusX0X1plusX0X0 = True
    Y0Y1 ≤ X1Y1plusX0Y1plusY1Y0 = True
    Y0Y1 ≤ X1Y1plusX0Y1plusY0Y0 = True
    Y0Y1 ≤ X1Y0plusX0Y0plusY0Y1 = True

    Y0Y0 ≤ OneOne = True
    Y0Y0 ≤ Y0Y0 = True
    Y0Y0 ≤ X1Y1plusY0Y0 = True
    Y0Y0 ≤ X1Y0plusX0Y0 = True
    Y0Y0 ≤ X0Y1plusY0Y0 = True
    Y0Y0 ≤ Y1X1plusY0Y0 = True
    Y0Y0 ≤ Y1X0plusY0Y0 = True
    Y0Y0 ≤ Y1Y1plusY0Y0 = True
    Y0Y0 ≤ Y0X1plusY0X0 = True
    Y0Y0 ≤ X1X1plusX1X0plusX0Y0 = True
    Y0Y0 ≤ X1X1plusX0X1plusY0X0 = True
    Y0Y0 ≤ X1X0plusX0X0plusY0X1 = True
    Y0Y0 ≤ X1Y1plusX0Y1plusY0Y0 = True
    Y0Y0 ≤ X1Y0plusX0X1plusX0X0 = True
    Y0Y0 ≤ X1Y0plusX0Y0plusY1Y1 = True
    Y0Y0 ≤ X1Y0plusX0Y0plusY0Y1 = True

    X1X1plusX1X0 ≤ OneOne = True
    X1X1plusX1X0 ≤ X1X1plusX1X0 = True
    X1X1plusX1X0 ≤ X1X1plusX1X0plusX0X1 = True
    X1X1plusX1X0 ≤ X1X1plusX1X0plusX0X0 = True
    X1X1plusX1X0 ≤ X1X1plusX1X0plusX0Y1 = True
    X1X1plusX1X0 ≤ X1X1plusX1X0plusX0Y0 = True

    X1X1plusX0X1 ≤ OneOne = True
    X1X1plusX0X1 ≤ X1X1plusX0X1 = True
    X1X1plusX0X1 ≤ X1X1plusX1X0plusX0X1 = True
    X1X1plusX0X1 ≤ X1X1plusX0X1plusX0X0 = True
    X1X1plusX0X1 ≤ X1X1plusX0X1plusY1X0 = True
    X1X1plusX0X1 ≤ X1X1plusX0X1plusY0X0 = True

    X1X1plusX0X0 ≤ OneOne = True
    X1X1plusX0X0 ≤ X1X1plusX0X0 = True
    X1X1plusX0X0 ≤ X1X1plusX1X0plusX0X0 = True
    X1X1plusX0X0 ≤ X1X1plusX0X1plusX0X0 = True

    X1X1plusX0Y1 ≤ OneOne = True
    X1X1plusX0Y1 ≤ X1X1plusX0Y1 = True
    X1X1plusX0Y1 ≤ X1X1plusX1X0plusX0Y1 = True
    X1X1plusX0Y1 ≤ X1X1plusX0X1plusX0X0 = True

    X1X1plusX0Y0 ≤ OneOne = True
    X1X1plusX0Y0 ≤ X1X1plusX0Y0 = True
    X1X1plusX0Y0 ≤ X1X1plusX1X0plusX0Y0 = True
    X1X1plusX0Y0 ≤ X1X1plusX0X1plusX0X0 = True

    X1X1plusY1X0 ≤ OneOne = True
    X1X1plusY1X0 ≤ X1X1plusY1X0 = True
    X1X1plusY1X0 ≤ X1X1plusX1X0plusX0X0 = True
    X1X1plusY1X0 ≤ X1X1plusX0X1plusY1X0 = True

    X1X1plusY0X0 ≤ OneOne = True
    X1X1plusY0X0 ≤ X1X1plusY0X0 = True
    X1X1plusY0X0 ≤ X1X1plusX1X0plusX0X0 = True
    X1X1plusY0X0 ≤ X1X1plusX0X1plusY0X0 = True

    X1X0plusX0X1 ≤ OneOne = True
    X1X0plusX0X1 ≤ X1X0plusX0X1 = True
    X1X0plusX0X1 ≤ X1X1plusX1X0plusX0X1 = True
    X1X0plusX0X1 ≤ X1X0plusX0X1plusX0X0 = True

    X1X0plusX0X0 ≤ OneOne = True
    X1X0plusX0X0 ≤ X1X0plusX0X0 = True
    X1X0plusX0X0 ≤ X1X1plusX1X0plusX0X0 = True
    X1X0plusX0X0 ≤ X1X0plusX0X1plusX0X0 = True
    X1X0plusX0X0 ≤ X1X0plusX0X0plusY1X1 = True
    X1X0plusX0X0 ≤ X1X0plusX0X0plusY0X1 = True

    X1X0plusX0Y1 ≤ OneOne = True
    X1X0plusX0Y1 ≤ X1X0plusX0Y1 = True
    X1X0plusX0Y1 ≤ X1X1plusX1X0plusX0Y1 = True
    X1X0plusX0Y1 ≤ X1X0plusX0X1plusX0X0 = True

    X1X0plusX0Y0 ≤ OneOne = True
    X1X0plusX0Y0 ≤ X1X0plusX0Y0 = True
    X1X0plusX0Y0 ≤ X1X1plusX1X0plusX0Y0 = True
    X1X0plusX0Y0 ≤ X1X0plusX0X1plusX0X0 = True

    X1X0plusY1X1 ≤ OneOne = True
    X1X0plusY1X1 ≤ X1X0plusY1X1 = True
    X1X0plusY1X1 ≤ X1X1plusX1X0plusX0X1 = True
    X1X0plusY1X1 ≤ X1X0plusX0X0plusY1X1 = True

    X1X0plusY0X1 ≤ OneOne = True
    X1X0plusY0X1 ≤ X1X0plusY0X1 = True
    X1X0plusY0X1 ≤ X1X1plusX1X0plusX0X1 = True
    X1X0plusY0X1 ≤ X1X0plusX0X0plusY0X1 = True

    X1Y1plusX0X1 ≤ OneOne = True
    X1Y1plusX0X1 ≤ X1Y1plusX0X1 = True
    X1Y1plusX0X1 ≤ X1X1plusX1X0plusX0X1 = True
    X1Y1plusX0X1 ≤ X1Y1plusX0X1plusX0X0 = True

    X1Y1plusX0X0 ≤ OneOne = True
    X1Y1plusX0X0 ≤ X1Y1plusX0X0 = True
    X1Y1plusX0X0 ≤ X1X1plusX1X0plusX0X0 = True
    X1Y1plusX0X0 ≤ X1Y1plusX0X1plusX0X0 = True

    X1Y1plusX0Y1 ≤ OneOne = True
    X1Y1plusX0Y1 ≤ X1Y1plusX0Y1 = True
    X1Y1plusX0Y1 ≤ X1X1plusX1X0plusX0Y1 = True
    X1Y1plusX0Y1 ≤ X1Y1plusX0X1plusX0X0 = True
    X1Y1plusX0Y1 ≤ X1Y1plusX0Y1plusY1Y0 = True
    X1Y1plusX0Y1 ≤ X1Y1plusX0Y1plusY0Y0 = True

    X1Y1plusX0Y0 ≤ OneOne = True
    X1Y1plusX0Y0 ≤ X1Y1plusX0Y0 = True
    X1Y1plusX0Y0 ≤ X1X1plusX1X0plusX0Y0 = True
    X1Y1plusX0Y0 ≤ X1Y1plusX0X1plusX0X0 = True

    X1Y1plusY1Y0 ≤ OneOne = True
    X1Y1plusY1Y0 ≤ X1Y1plusY1Y0 = True
    X1Y1plusY1Y0 ≤ X1X1plusX1X0plusX0Y0 = True
    X1Y1plusY1Y0 ≤ X1Y1plusX0Y1plusY1Y0 = True

    X1Y1plusY0Y0 ≤ OneOne = True
    X1Y1plusY0Y0 ≤ X1Y1plusY0Y0 = True
    X1Y1plusY0Y0 ≤ X1X1plusX1X0plusX0Y0 = True
    X1Y1plusY0Y0 ≤ X1Y1plusX0Y1plusY0Y0 = True

    X1Y0plusX0X1 ≤ OneOne = True
    X1Y0plusX0X1 ≤ X1Y0plusX0X1 = True
    X1Y0plusX0X1 ≤ X1X1plusX1X0plusX0X1 = True
    X1Y0plusX0X1 ≤ X1Y0plusX0X1plusX0X0 = True

    X1Y0plusX0X0 ≤ OneOne = True
    X1Y0plusX0X0 ≤ X1Y0plusX0X0 = True
    X1Y0plusX0X0 ≤ X1X1plusX1X0plusX0X0 = True
    X1Y0plusX0X0 ≤ X1Y0plusX0X1plusX0X0 = True

    X1Y0plusX0Y1 ≤ OneOne = True
    X1Y0plusX0Y1 ≤ X1Y0plusX0Y1 = True
    X1Y0plusX0Y1 ≤ X1X1plusX1X0plusX0Y1 = True
    X1Y0plusX0Y1 ≤ X1Y0plusX0X1plusX0X0 = True

    X1Y0plusX0Y0 ≤ OneOne = True
    X1Y0plusX0Y0 ≤ X1Y0plusX0Y0 = True
    X1Y0plusX0Y0 ≤ X1X1plusX1X0plusX0Y0 = True
    X1Y0plusX0Y0 ≤ X1Y0plusX0X1plusX0X0 = True
    X1Y0plusX0Y0 ≤ X1Y0plusX0Y0plusY1Y1 = True
    X1Y0plusX0Y0 ≤ X1Y0plusX0Y0plusY0Y1 = True

    X1Y0plusY1Y1 ≤ OneOne = True
    X1Y0plusY1Y1 ≤ X1Y0plusY1Y1 = True
    X1Y0plusY1Y1 ≤ X1X1plusX1X0plusX0Y1 = True
    X1Y0plusY1Y1 ≤ X1Y0plusX0Y0plusY1Y1 = True

    X1Y0plusY0Y1 ≤ OneOne = True
    X1Y0plusY0Y1 ≤ X1Y0plusY0Y1 = True
    X1Y0plusY0Y1 ≤ X1X1plusX1X0plusX0Y1 = True
    X1Y0plusY0Y1 ≤ X1Y0plusX0Y0plusY0Y1 = True

    X0X1plusX0X0 ≤ OneOne = True
    X0X1plusX0X0 ≤ X0X1plusX0X0 = True
    X0X1plusX0X0 ≤ X1X1plusX0X1plusX0X0 = True
    X0X1plusX0X0 ≤ X1X0plusX0X1plusX0X0 = True
    X0X1plusX0X0 ≤ X1Y1plusX0X1plusX0X0 = True
    X0X1plusX0X0 ≤ X1Y0plusX0X1plusX0X0 = True

    X0X1plusY1X0 ≤ OneOne = True
    X0X1plusY1X0 ≤ X0X1plusY1X0 = True
    X0X1plusY1X0 ≤ X1X1plusX0X1plusY1X0 = True
    X0X1plusY1X0 ≤ X1X0plusX0X1plusX0X0 = True

    X0X1plusY0X0 ≤ OneOne = True
    X0X1plusY0X0 ≤ X0X1plusY0X0 = True
    X0X1plusY0X0 ≤ X1X1plusX0X1plusY0X0 = True
    X0X1plusY0X0 ≤ X1X0plusX0X1plusX0X0 = True

    X0X0plusY1X1 ≤ OneOne = True
    X0X0plusY1X1 ≤ X0X0plusY1X1 = True
    X0X0plusY1X1 ≤ X1X1plusX0X1plusX0X0 = True
    X0X0plusY1X1 ≤ X1X0plusX0X0plusY1X1 = True

    X0X0plusY0X1 ≤ OneOne = True
    X0X0plusY0X1 ≤ X0X0plusY0X1 = True
    X0X0plusY0X1 ≤ X1X1plusX0X1plusX0X0 = True
    X0X0plusY0X1 ≤ X1X0plusX0X0plusY0X1 = True

    X0Y1plusY1Y0 ≤ OneOne = True
    X0Y1plusY1Y0 ≤ X0Y1plusY1Y0 = True
    X0Y1plusY1Y0 ≤ X1Y1plusX0Y1plusY1Y0 = True
    X0Y1plusY1Y0 ≤ X1Y0plusX0X1plusX0X0 = True

    X0Y1plusY0Y0 ≤ OneOne = True
    X0Y1plusY0Y0 ≤ X0Y1plusY0Y0 = True
    X0Y1plusY0Y0 ≤ X1Y1plusX0Y1plusY0Y0 = True
    X0Y1plusY0Y0 ≤ X1Y0plusX0X1plusX0X0 = True

    X0Y0plusY1Y1 ≤ OneOne = True
    X0Y0plusY1Y1 ≤ X0Y0plusY1Y1 = True
    X0Y0plusY1Y1 ≤ X1Y1plusX0X1plusX0X0 = True
    X0Y0plusY1Y1 ≤ X1Y0plusX0Y0plusY1Y1 = True

    X0Y0plusY0Y1 ≤ OneOne = True
    X0Y0plusY0Y1 ≤ X0Y0plusY0Y1 = True
    X0Y0plusY0Y1 ≤ X1Y1plusX0X1plusX0X0 = True
    X0Y0plusY0Y1 ≤ X1Y0plusX0Y0plusY0Y1 = True

    Y1X1plusY1X0 ≤ OneOne = True
    Y1X1plusY1X0 ≤ Y1X1plusY1X0 = True
    Y1X1plusY1X0 ≤ X1X1plusX0X1plusY1X0 = True
    Y1X1plusY1X0 ≤ X1X0plusX0X0plusY1X1 = True
    Y1X1plusY1X0 ≤ X1Y1plusX0Y1plusY1Y0 = True
    Y1X1plusY1X0 ≤ X1Y0plusX0Y0plusY1Y1 = True

    Y1X1plusY0X0 ≤ OneOne = True
    Y1X1plusY0X0 ≤ Y1X1plusY0X0 = True
    Y1X1plusY0X0 ≤ X1X1plusX0X1plusY0X0 = True
    Y1X1plusY0X0 ≤ X1X0plusX0X0plusY1X1 = True

    Y1X1plusY0Y1 ≤ OneOne = True
    Y1X1plusY0Y1 ≤ Y1X1plusY0Y1 = True
    Y1X1plusY0Y1 ≤ X1X1plusX0X1plusY0X0 = True
    Y1X1plusY0Y1 ≤ X1Y1plusX0Y1plusY1Y0 = True

    Y1X1plusY0Y0 ≤ OneOne = True
    Y1X1plusY0Y0 ≤ Y1X1plusY0Y0 = True
    Y1X1plusY0Y0 ≤ X1X1plusX0X1plusY0X0 = True
    Y1X1plusY0Y0 ≤ X1Y0plusX0Y0plusY1Y1 = True

    Y1X0plusY0X1 ≤ OneOne = True
    Y1X0plusY0X1 ≤ Y1X0plusY0X1 = True
    Y1X0plusY0X1 ≤ X1X1plusX0X1plusY1X0 = True
    Y1X0plusY0X1 ≤ X1X0plusX0X0plusY0X1 = True

    Y1X0plusY0Y1 ≤ OneOne = True
    Y1X0plusY0Y1 ≤ Y1X0plusY0Y1 = True
    Y1X0plusY0Y1 ≤ X1X0plusX0X0plusY0X1 = True
    Y1X0plusY0Y1 ≤ X1Y1plusX0Y1plusY1Y0 = True

    Y1X0plusY0Y0 ≤ OneOne = True
    Y1X0plusY0Y0 ≤ Y1X0plusY0Y0 = True
    Y1X0plusY0Y0 ≤ X1X0plusX0X0plusY0X1 = True
    Y1X0plusY0Y0 ≤ X1Y0plusX0Y0plusY1Y1 = True

    Y1Y1plusY0X1 ≤ OneOne = True
    Y1Y1plusY0X1 ≤ Y1Y1plusY0X1 = True
    Y1Y1plusY0X1 ≤ X1X1plusX0X1plusY1X0 = True
    Y1Y1plusY0X1 ≤ X1Y1plusX0Y1plusY0Y0 = True

    Y1Y1plusY0X0 ≤ OneOne = True
    Y1Y1plusY0X0 ≤ Y1Y1plusY0X0 = True
    Y1Y1plusY0X0 ≤ X1X0plusX0X0plusY1X1 = True
    Y1Y1plusY0X0 ≤ X1Y1plusX0Y1plusY0Y0 = True

    Y1Y1plusY0Y0 ≤ OneOne = True
    Y1Y1plusY0Y0 ≤ Y1Y1plusY0Y0 = True
    Y1Y1plusY0Y0 ≤ X1Y1plusX0Y1plusY0Y0 = True
    Y1Y1plusY0Y0 ≤ X1Y0plusX0Y0plusY1Y1 = True

    Y1Y0plusY0X1 ≤ OneOne = True
    Y1Y0plusY0X1 ≤ Y1Y0plusY0X1 = True
    Y1Y0plusY0X1 ≤ X1X1plusX0X1plusY1X0 = True
    Y1Y0plusY0X1 ≤ X1Y0plusX0Y0plusY0Y1 = True

    Y1Y0plusY0X0 ≤ OneOne = True
    Y1Y0plusY0X0 ≤ Y1Y0plusY0X0 = True
    Y1Y0plusY0X0 ≤ X1X0plusX0X0plusY1X1 = True
    Y1Y0plusY0X0 ≤ X1Y0plusX0Y0plusY0Y1 = True

    Y1Y0plusY0Y1 ≤ OneOne = True
    Y1Y0plusY0Y1 ≤ Y1Y0plusY0Y1 = True
    Y1Y0plusY0Y1 ≤ X1Y1plusX0Y1plusY1Y0 = True
    Y1Y0plusY0Y1 ≤ X1Y0plusX0Y0plusY0Y1 = True

    Y0X1plusY0X0 ≤ OneOne = True
    Y0X1plusY0X0 ≤ Y0X1plusY0X0 = True
    Y0X1plusY0X0 ≤ X1X1plusX0X1plusY0X0 = True
    Y0X1plusY0X0 ≤ X1X0plusX0X0plusY0X1 = True
    Y0X1plusY0X0 ≤ X1Y1plusX0Y1plusY0Y0 = True
    Y0X1plusY0X0 ≤ X1Y0plusX0Y0plusY0Y1 = True

    X1X1plusX1X0plusX0X1 ≤ OneOne = True
    X1X1plusX1X0plusX0X1 ≤ X1X1plusX1X0plusX0X1 = True

    X1X1plusX1X0plusX0X0 ≤ OneOne = True
    X1X1plusX1X0plusX0X0 ≤ X1X1plusX1X0plusX0X0 = True

    X1X1plusX1X0plusX0Y1 ≤ OneOne = True
    X1X1plusX1X0plusX0Y1 ≤ X1X1plusX1X0plusX0Y1 = True

    X1X1plusX1X0plusX0Y0 ≤ OneOne = True
    X1X1plusX1X0plusX0Y0 ≤ X1X1plusX1X0plusX0Y0 = True

    X1X1plusX0X1plusX0X0 ≤ OneOne = True
    X1X1plusX0X1plusX0X0 ≤ X1X1plusX0X1plusX0X0 = True

    X1X1plusX0X1plusY1X0 ≤ OneOne = True
    X1X1plusX0X1plusY1X0 ≤ X1X1plusX0X1plusY1X0 = True

    X1X1plusX0X1plusY0X0 ≤ OneOne = True
    X1X1plusX0X1plusY0X0 ≤ X1X1plusX0X1plusY0X0 = True

    X1X0plusX0X1plusX0X0 ≤ OneOne = True
    X1X0plusX0X1plusX0X0 ≤ X1X0plusX0X1plusX0X0 = True

    X1X0plusX0X0plusY1X1 ≤ OneOne = True
    X1X0plusX0X0plusY1X1 ≤ X1X0plusX0X0plusY1X1 = True

    X1X0plusX0X0plusY0X1 ≤ OneOne = True
    X1X0plusX0X0plusY0X1 ≤ X1X0plusX0X0plusY0X1 = True

    X1Y1plusX0X1plusX0X0 ≤ OneOne = True
    X1Y1plusX0X1plusX0X0 ≤ X1Y1plusX0X1plusX0X0 = True

    X1Y1plusX0Y1plusY1Y0 ≤ OneOne = True
    X1Y1plusX0Y1plusY1Y0 ≤ X1Y1plusX0Y1plusY1Y0 = True

    X1Y1plusX0Y1plusY0Y0 ≤ OneOne = True
    X1Y1plusX0Y1plusY0Y0 ≤ X1Y1plusX0Y1plusY0Y0 = True

    X1Y0plusX0X1plusX0X0 ≤ OneOne = True
    X1Y0plusX0X1plusX0X0 ≤ X1Y0plusX0X1plusX0X0 = True

    X1Y0plusX0Y0plusY1Y1 ≤ OneOne = True
    X1Y0plusX0Y0plusY1Y1 ≤ X1Y0plusX0Y0plusY1Y1 = True

    X1Y0plusX0Y0plusY0Y1 ≤ OneOne = True
    X1Y0plusX0Y0plusY0Y1 ≤ X1Y0plusX0Y0plusY0Y1 = True

    _ ≤ _ = False

twoTwoBoxOrtho :: TwoTwoBoxWorld -> TwoTwoBoxWorld
twoTwoBoxOrtho ZeroZero = OneOne
twoTwoBoxOrtho OneOne = ZeroZero
twoTwoBoxOrtho X1X1 = X1X0plusX0X1plusX0X0
twoTwoBoxOrtho X1X0 = X1X1plusX0X1plusX0X0
twoTwoBoxOrtho X1Y1 = X1Y0plusX0X1plusX0X0
twoTwoBoxOrtho X1Y0 = X1Y1plusX0X1plusX0X0
twoTwoBoxOrtho X0X1 = X1X1plusX1X0plusX0X0
twoTwoBoxOrtho X0X0 = X1X1plusX1X0plusX0X1
twoTwoBoxOrtho X0Y1 = X1X1plusX1X0plusX0Y0
twoTwoBoxOrtho X0Y0 = X1X1plusX1X0plusX0Y1
twoTwoBoxOrtho Y1X1 = X1X0plusX0X0plusY0X1
twoTwoBoxOrtho Y1X0 = X1X1plusX0X1plusY0X0
twoTwoBoxOrtho Y1Y1 = X1Y0plusX0Y0plusY0Y1
twoTwoBoxOrtho Y1Y0 = X1Y1plusX0Y1plusY0Y0
twoTwoBoxOrtho Y0X1 = X1X0plusX0X0plusY1X1
twoTwoBoxOrtho Y0X0 = X1X1plusX0X1plusY1X0
twoTwoBoxOrtho Y0Y1 = X1Y0plusX0Y0plusY1Y1
twoTwoBoxOrtho Y0Y0 = X1Y1plusX0Y1plusY1Y0
twoTwoBoxOrtho X1X1plusX1X0 = X0X1plusX0X0
twoTwoBoxOrtho X1X1plusX0X1 = X1X0plusX0X0
twoTwoBoxOrtho X1X1plusX0X0 = X1X0plusX0X1
twoTwoBoxOrtho X1X1plusX0Y1 = X1X0plusX0Y0
twoTwoBoxOrtho X1X1plusX0Y0 = X1X0plusX0Y1
twoTwoBoxOrtho X1X1plusY1X0 = X0X1plusY0X0
twoTwoBoxOrtho X1X1plusY0X0 = X0X1plusY1X0
twoTwoBoxOrtho X1X0plusX0X1 = X1X1plusX0X0
twoTwoBoxOrtho X1X0plusX0X0 = X1X1plusX0X1
twoTwoBoxOrtho X1X0plusX0Y1 = X1X1plusX0Y0
twoTwoBoxOrtho X1X0plusX0Y0 = X1X1plusX0Y1
twoTwoBoxOrtho X1X0plusY1X1 = X0X0plusY0X1
twoTwoBoxOrtho X1X0plusY0X1 = X0X0plusY1X1
twoTwoBoxOrtho X1Y1plusX0X1 = X1Y0plusX0X0
twoTwoBoxOrtho X1Y1plusX0X0 = X1Y0plusX0X1
twoTwoBoxOrtho X1Y1plusX0Y1 = X1Y0plusX0Y0
twoTwoBoxOrtho X1Y1plusX0Y0 = X1Y0plusX0Y1
twoTwoBoxOrtho X1Y1plusY1Y0 = X0Y1plusY0Y0
twoTwoBoxOrtho X1Y1plusY0Y0 = X0Y1plusY1Y0
twoTwoBoxOrtho X1Y0plusX0X1 = X1Y1plusX0X0
twoTwoBoxOrtho X1Y0plusX0X0 = X1Y1plusX0X1
twoTwoBoxOrtho X1Y0plusX0Y1 = X1Y1plusX0Y0
twoTwoBoxOrtho X1Y0plusX0Y0 = X1Y1plusX0Y1
twoTwoBoxOrtho X1Y0plusY1Y1 = X0Y0plusY0Y1
twoTwoBoxOrtho X1Y0plusY0Y1 = X0Y0plusY1Y1
twoTwoBoxOrtho X0X1plusX0X0 = X1X1plusX1X0
twoTwoBoxOrtho X0X1plusY1X0 = X1X1plusY0X0
twoTwoBoxOrtho X0X1plusY0X0 = X1X1plusY1X0
twoTwoBoxOrtho X0X0plusY1X1 = X1X0plusY0X1
twoTwoBoxOrtho X0X0plusY0X1 = X1X0plusY1X1
twoTwoBoxOrtho X0Y1plusY1Y0 = X1Y1plusY0Y0
twoTwoBoxOrtho X0Y1plusY0Y0 = X1Y1plusY1Y0
twoTwoBoxOrtho X0Y0plusY1Y1 = X1Y0plusY0Y1
twoTwoBoxOrtho X0Y0plusY0Y1 = X1Y0plusY1Y1
twoTwoBoxOrtho Y1X1plusY1X0 = Y0X1plusY0X0
twoTwoBoxOrtho Y1X1plusY0X0 = Y1X0plusY0X1
twoTwoBoxOrtho Y1X1plusY0Y1 = Y1X0plusY0Y0
twoTwoBoxOrtho Y1X1plusY0Y0 = Y1X0plusY0Y1
twoTwoBoxOrtho Y1X0plusY0X1 = Y1X1plusY0X0
twoTwoBoxOrtho Y1X0plusY0Y1 = Y1X1plusY0Y0
twoTwoBoxOrtho Y1X0plusY0Y0 = Y1X1plusY0Y1
twoTwoBoxOrtho Y1Y1plusY0X1 = Y1Y0plusY0X0
twoTwoBoxOrtho Y1Y1plusY0X0 = Y1Y0plusY0X1
twoTwoBoxOrtho Y1Y1plusY0Y0 = Y1Y0plusY0Y1
twoTwoBoxOrtho Y1Y0plusY0X1 = Y1Y1plusY0X0
twoTwoBoxOrtho Y1Y0plusY0X0 = Y1Y1plusY0X1
twoTwoBoxOrtho Y1Y0plusY0Y1 = Y1Y1plusY0Y0
twoTwoBoxOrtho Y0X1plusY0X0 = Y1X1plusY1X0
twoTwoBoxOrtho X1X1plusX1X0plusX0X1 = X0X0
twoTwoBoxOrtho X1X1plusX1X0plusX0X0 = X0X1
twoTwoBoxOrtho X1X1plusX1X0plusX0Y1 = X0Y0
twoTwoBoxOrtho X1X1plusX1X0plusX0Y0 = X0Y1
twoTwoBoxOrtho X1X1plusX0X1plusX0X0 = X1X0
twoTwoBoxOrtho X1X1plusX0X1plusY1X0 = Y0X0
twoTwoBoxOrtho X1X1plusX0X1plusY0X0 = Y1X0
twoTwoBoxOrtho X1X0plusX0X1plusX0X0 = X1X1
twoTwoBoxOrtho X1X0plusX0X0plusY1X1 = Y0X1
twoTwoBoxOrtho X1X0plusX0X0plusY0X1 = Y1X1
twoTwoBoxOrtho X1Y1plusX0X1plusX0X0 = X1Y0
twoTwoBoxOrtho X1Y1plusX0Y1plusY1Y0 = Y0Y0
twoTwoBoxOrtho X1Y1plusX0Y1plusY0Y0 = Y1Y0
twoTwoBoxOrtho X1Y0plusX0X1plusX0X0 = X1Y1
twoTwoBoxOrtho X1Y0plusX0Y0plusY1Y1 = Y0Y1
twoTwoBoxOrtho X1Y0plusX0Y0plusY0Y1 = Y1Y1
