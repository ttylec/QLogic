module WTest where

import Data.Poset

data WTest = Y1Y1 | Y1Y0 | Y1X1 | Y1One | Y1X0 | Y0Y1PLUSY1X0 | Y0Y1PLUSY1X1 | Y0Y1PLUSY1Y0 | Y0Y1 | Y0Y0PLUSY1X0 | Y0Y0PLUSY1X1 | Y0Y0PLUSY1Y1 | Y0Y0 | Y0X1PLUSY1X0 | Y0X1PLUSY1Y0 | Y0X1PLUSY1Y1 | Y0X1 | Y0One | Y0X0PLUSY1X1 | Y0X0PLUSY1Y0 | Y0X0PLUSY1Y1 | Y0X0 | X1Y1PLUSY0Y0 | X1Y1PLUSY1Y0 | X1Y1 | X1Y0PLUSY0Y1 | X1Y0PLUSY1Y1 | X1Y0 | X1X1PLUSY0X0 | X1X1PLUSY1X0 | X1X1 | X1One | X1X0PLUSY0X1 | X1X0PLUSY1X1 | X1X0 | X0Y1PLUSX1X0 | X0Y1PLUSX1X1 | X0Y1PLUSX1Y0 | X0Y1PLUSX1Y1PLUSY0Y0 | X0Y1PLUSX1Y1PLUSY1Y0 | OneY1 | X0Y1PLUSX1One | X0Y1PLUSY0Y0 | X0Y1PLUSY1Y0 | X0Y1 | X0Y0PLUSX1X0 | X0Y0PLUSX1X1 | X0Y0PLUSX1Y0PLUSY0Y1 | X0Y0PLUSX1Y0PLUSY1Y1 | OneY0 | X0Y0PLUSX1Y1 | X0Y0PLUSX1One | X0Y0PLUSY0Y1 | X0Y0PLUSY1Y1 | X0Y0 | X0X1PLUSX1X0 | X0X1PLUSX1X1PLUSY0X0 | X0X1PLUSX1X1PLUSY1X0 | OneX1 | X0X1PLUSX1Y0 | X0X1PLUSX1Y1 | X0X1PLUSX1One | X0X1PLUSY0X0 | X0X1PLUSY1X0 | X0X1 | X0X0PLUSX0X1PLUSX1X0 | X0X0PLUSX0X1PLUSX1Y0 | X0X0PLUSX0X1PLUSX1Y1 | X0One | X0X0PLUSX1X0PLUSY0X1 | X0X0PLUSX1X0PLUSY1X1 | OneOne | OneX0 | X0X0PLUSX1X1 | X0X0PLUSX1Y0 | X0X0PLUSX1Y1 | X0X0PLUSX1One | X0X0PLUSY0X1 | X0X0PLUSY1X1 | X0X0PLUSOneX1 | X0X0 | ZeroZero deriving (Bounded, Enum, Eq, Ord, Show)
instance POrd WTest where
    Y1Y1 .<=. Y1Y1 = True 
    Y1Y1 .<=. Y1One = True 
    Y1Y1 .<=. Y0Y0PLUSY1Y1 = True 
    Y1Y1 .<=. Y0X1PLUSY1Y1 = True 
    Y1Y1 .<=. Y0X0PLUSY1Y1 = True 
    Y1Y1 .<=. X1Y0PLUSY1Y1 = True 
    Y1Y1 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    Y1Y1 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    Y1Y1 .<=. OneY1 = True 
    Y1Y1 .<=. X0Y1PLUSX1One = True 
    Y1Y1 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    Y1Y1 .<=. X0Y0PLUSY1Y1 = True 
    Y1Y1 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    Y1Y1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    Y1Y1 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    Y1Y1 .<=. OneOne = True 

    Y1Y0 .<=. Y1Y0 = True 
    Y1Y0 .<=. Y1One = True 
    Y1Y0 .<=. Y0Y1PLUSY1Y0 = True 
    Y1Y0 .<=. Y0X1PLUSY1Y0 = True 
    Y1Y0 .<=. Y0X0PLUSY1Y0 = True 
    Y1Y0 .<=. X1Y1PLUSY1Y0 = True 
    Y1Y0 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    Y1Y0 .<=. X0Y1PLUSY1Y0 = True 
    Y1Y0 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    Y1Y0 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    Y1Y0 .<=. OneY0 = True 
    Y1Y0 .<=. X0Y0PLUSX1One = True 
    Y1Y0 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    Y1Y0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    Y1Y0 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    Y1Y0 .<=. OneOne = True 

    Y1X1 .<=. Y1X1 = True 
    Y1X1 .<=. Y1One = True 
    Y1X1 .<=. Y0Y1PLUSY1X1 = True 
    Y1X1 .<=. Y0Y0PLUSY1X1 = True 
    Y1X1 .<=. Y0X0PLUSY1X1 = True 
    Y1X1 .<=. X1X0PLUSY1X1 = True 
    Y1X1 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    Y1X1 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    Y1X1 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    Y1X1 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    Y1X1 .<=. OneX1 = True 
    Y1X1 .<=. X0X1PLUSX1One = True 
    Y1X1 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    Y1X1 .<=. OneOne = True 
    Y1X1 .<=. X0X0PLUSY1X1 = True 
    Y1X1 .<=. X0X0PLUSOneX1 = True 

    Y1One .<=. Y1One = True 
    Y1One .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    Y1One .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    Y1One .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    Y1One .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    Y1One .<=. OneOne = True 

    Y1X0 .<=. Y1One = True 
    Y1X0 .<=. Y1X0 = True 
    Y1X0 .<=. Y0Y1PLUSY1X0 = True 
    Y1X0 .<=. Y0Y0PLUSY1X0 = True 
    Y1X0 .<=. Y0X1PLUSY1X0 = True 
    Y1X0 .<=. X1X1PLUSY1X0 = True 
    Y1X0 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    Y1X0 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    Y1X0 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    Y1X0 .<=. X0X1PLUSY1X0 = True 
    Y1X0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    Y1X0 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    Y1X0 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    Y1X0 .<=. OneOne = True 
    Y1X0 .<=. OneX0 = True 
    Y1X0 .<=. X0X0PLUSX1One = True 

    Y0Y1PLUSY1X0 .<=. Y0Y1PLUSY1X0 = True 
    Y0Y1PLUSY1X0 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    Y0Y1PLUSY1X0 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    Y0Y1PLUSY1X0 .<=. OneOne = True 

    Y0Y1PLUSY1X1 .<=. Y0Y1PLUSY1X1 = True 
    Y0Y1PLUSY1X1 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    Y0Y1PLUSY1X1 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    Y0Y1PLUSY1X1 .<=. OneOne = True 

    Y0Y1PLUSY1Y0 .<=. Y0Y1PLUSY1Y0 = True 
    Y0Y1PLUSY1Y0 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    Y0Y1PLUSY1Y0 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    Y0Y1PLUSY1Y0 .<=. OneOne = True 

    Y0Y1 .<=. Y0Y1PLUSY1X0 = True 
    Y0Y1 .<=. Y0Y1PLUSY1X1 = True 
    Y0Y1 .<=. Y0Y1PLUSY1Y0 = True 
    Y0Y1 .<=. Y0Y1 = True 
    Y0Y1 .<=. Y0One = True 
    Y0Y1 .<=. X1Y0PLUSY0Y1 = True 
    Y0Y1 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    Y0Y1 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    Y0Y1 .<=. OneY1 = True 
    Y0Y1 .<=. X0Y1PLUSX1One = True 
    Y0Y1 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    Y0Y1 .<=. X0Y0PLUSY0Y1 = True 
    Y0Y1 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    Y0Y1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    Y0Y1 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    Y0Y1 .<=. OneOne = True 

    Y0Y0PLUSY1X0 .<=. Y0Y0PLUSY1X0 = True 
    Y0Y0PLUSY1X0 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    Y0Y0PLUSY1X0 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    Y0Y0PLUSY1X0 .<=. OneOne = True 

    Y0Y0PLUSY1X1 .<=. Y0Y0PLUSY1X1 = True 
    Y0Y0PLUSY1X1 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    Y0Y0PLUSY1X1 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    Y0Y0PLUSY1X1 .<=. OneOne = True 

    Y0Y0PLUSY1Y1 .<=. Y0Y0PLUSY1Y1 = True 
    Y0Y0PLUSY1Y1 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    Y0Y0PLUSY1Y1 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    Y0Y0PLUSY1Y1 .<=. OneOne = True 

    Y0Y0 .<=. Y0Y0PLUSY1X0 = True 
    Y0Y0 .<=. Y0Y0PLUSY1X1 = True 
    Y0Y0 .<=. Y0Y0PLUSY1Y1 = True 
    Y0Y0 .<=. Y0Y0 = True 
    Y0Y0 .<=. Y0One = True 
    Y0Y0 .<=. X1Y1PLUSY0Y0 = True 
    Y0Y0 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    Y0Y0 .<=. X0Y1PLUSY0Y0 = True 
    Y0Y0 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    Y0Y0 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    Y0Y0 .<=. OneY0 = True 
    Y0Y0 .<=. X0Y0PLUSX1One = True 
    Y0Y0 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    Y0Y0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    Y0Y0 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    Y0Y0 .<=. OneOne = True 

    Y0X1PLUSY1X0 .<=. Y0X1PLUSY1X0 = True 
    Y0X1PLUSY1X0 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    Y0X1PLUSY1X0 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    Y0X1PLUSY1X0 .<=. OneOne = True 

    Y0X1PLUSY1Y0 .<=. Y0X1PLUSY1Y0 = True 
    Y0X1PLUSY1Y0 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    Y0X1PLUSY1Y0 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    Y0X1PLUSY1Y0 .<=. OneOne = True 

    Y0X1PLUSY1Y1 .<=. Y0X1PLUSY1Y1 = True 
    Y0X1PLUSY1Y1 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    Y0X1PLUSY1Y1 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    Y0X1PLUSY1Y1 .<=. OneOne = True 

    Y0X1 .<=. Y0X1PLUSY1X0 = True 
    Y0X1 .<=. Y0X1PLUSY1Y0 = True 
    Y0X1 .<=. Y0X1PLUSY1Y1 = True 
    Y0X1 .<=. Y0X1 = True 
    Y0X1 .<=. Y0One = True 
    Y0X1 .<=. X1X0PLUSY0X1 = True 
    Y0X1 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    Y0X1 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    Y0X1 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    Y0X1 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    Y0X1 .<=. OneX1 = True 
    Y0X1 .<=. X0X1PLUSX1One = True 
    Y0X1 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    Y0X1 .<=. OneOne = True 
    Y0X1 .<=. X0X0PLUSY0X1 = True 
    Y0X1 .<=. X0X0PLUSOneX1 = True 

    Y0One .<=. Y0One = True 
    Y0One .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    Y0One .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    Y0One .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    Y0One .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    Y0One .<=. OneOne = True 

    Y0X0PLUSY1X1 .<=. Y0X0PLUSY1X1 = True 
    Y0X0PLUSY1X1 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    Y0X0PLUSY1X1 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    Y0X0PLUSY1X1 .<=. OneOne = True 

    Y0X0PLUSY1Y0 .<=. Y0X0PLUSY1Y0 = True 
    Y0X0PLUSY1Y0 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    Y0X0PLUSY1Y0 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    Y0X0PLUSY1Y0 .<=. OneOne = True 

    Y0X0PLUSY1Y1 .<=. Y0X0PLUSY1Y1 = True 
    Y0X0PLUSY1Y1 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    Y0X0PLUSY1Y1 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    Y0X0PLUSY1Y1 .<=. OneOne = True 

    Y0X0 .<=. Y0One = True 
    Y0X0 .<=. Y0X0PLUSY1X1 = True 
    Y0X0 .<=. Y0X0PLUSY1Y0 = True 
    Y0X0 .<=. Y0X0PLUSY1Y1 = True 
    Y0X0 .<=. Y0X0 = True 
    Y0X0 .<=. X1X1PLUSY0X0 = True 
    Y0X0 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    Y0X0 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    Y0X0 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    Y0X0 .<=. X0X1PLUSY0X0 = True 
    Y0X0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    Y0X0 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    Y0X0 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    Y0X0 .<=. OneOne = True 
    Y0X0 .<=. OneX0 = True 
    Y0X0 .<=. X0X0PLUSX1One = True 

    X1Y1PLUSY0Y0 .<=. X1Y1PLUSY0Y0 = True 
    X1Y1PLUSY0Y0 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    X1Y1PLUSY0Y0 .<=. X0Y0PLUSX1One = True 
    X1Y1PLUSY0Y0 .<=. OneOne = True 

    X1Y1PLUSY1Y0 .<=. X1Y1PLUSY1Y0 = True 
    X1Y1PLUSY1Y0 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    X1Y1PLUSY1Y0 .<=. X0Y0PLUSX1One = True 
    X1Y1PLUSY1Y0 .<=. OneOne = True 

    X1Y1 .<=. X1Y1PLUSY0Y0 = True 
    X1Y1 .<=. X1Y1PLUSY1Y0 = True 
    X1Y1 .<=. X1Y1 = True 
    X1Y1 .<=. X1One = True 
    X1Y1 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    X1Y1 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    X1Y1 .<=. OneY1 = True 
    X1Y1 .<=. X0Y1PLUSX1One = True 
    X1Y1 .<=. X0Y0PLUSX1Y1 = True 
    X1Y1 .<=. X0Y0PLUSX1One = True 
    X1Y1 .<=. X0X1PLUSX1Y1 = True 
    X1Y1 .<=. X0X1PLUSX1One = True 
    X1Y1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X1Y1 .<=. OneOne = True 
    X1Y1 .<=. X0X0PLUSX1Y1 = True 
    X1Y1 .<=. X0X0PLUSX1One = True 

    X1Y0PLUSY0Y1 .<=. X1Y0PLUSY0Y1 = True 
    X1Y0PLUSY0Y1 .<=. X0Y1PLUSX1One = True 
    X1Y0PLUSY0Y1 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    X1Y0PLUSY0Y1 .<=. OneOne = True 

    X1Y0PLUSY1Y1 .<=. X1Y0PLUSY1Y1 = True 
    X1Y0PLUSY1Y1 .<=. X0Y1PLUSX1One = True 
    X1Y0PLUSY1Y1 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    X1Y0PLUSY1Y1 .<=. OneOne = True 

    X1Y0 .<=. X1Y0PLUSY0Y1 = True 
    X1Y0 .<=. X1Y0PLUSY1Y1 = True 
    X1Y0 .<=. X1Y0 = True 
    X1Y0 .<=. X1One = True 
    X1Y0 .<=. X0Y1PLUSX1Y0 = True 
    X1Y0 .<=. X0Y1PLUSX1One = True 
    X1Y0 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    X1Y0 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    X1Y0 .<=. OneY0 = True 
    X1Y0 .<=. X0Y0PLUSX1One = True 
    X1Y0 .<=. X0X1PLUSX1Y0 = True 
    X1Y0 .<=. X0X1PLUSX1One = True 
    X1Y0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X1Y0 .<=. OneOne = True 
    X1Y0 .<=. X0X0PLUSX1Y0 = True 
    X1Y0 .<=. X0X0PLUSX1One = True 

    X1X1PLUSY0X0 .<=. X1X1PLUSY0X0 = True 
    X1X1PLUSY0X0 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    X1X1PLUSY0X0 .<=. OneOne = True 
    X1X1PLUSY0X0 .<=. X0X0PLUSX1One = True 

    X1X1PLUSY1X0 .<=. X1X1PLUSY1X0 = True 
    X1X1PLUSY1X0 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    X1X1PLUSY1X0 .<=. OneOne = True 
    X1X1PLUSY1X0 .<=. X0X0PLUSX1One = True 

    X1X1 .<=. X1X1PLUSY0X0 = True 
    X1X1 .<=. X1X1PLUSY1X0 = True 
    X1X1 .<=. X1X1 = True 
    X1X1 .<=. X1One = True 
    X1X1 .<=. X0Y1PLUSX1X1 = True 
    X1X1 .<=. X0Y1PLUSX1One = True 
    X1X1 .<=. X0Y0PLUSX1X1 = True 
    X1X1 .<=. X0Y0PLUSX1One = True 
    X1X1 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    X1X1 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    X1X1 .<=. OneX1 = True 
    X1X1 .<=. X0X1PLUSX1One = True 
    X1X1 .<=. OneOne = True 
    X1X1 .<=. X0X0PLUSX1X1 = True 
    X1X1 .<=. X0X0PLUSX1One = True 
    X1X1 .<=. X0X0PLUSOneX1 = True 

    X1One .<=. X1One = True 
    X1One .<=. X0Y1PLUSX1One = True 
    X1One .<=. X0Y0PLUSX1One = True 
    X1One .<=. X0X1PLUSX1One = True 
    X1One .<=. OneOne = True 
    X1One .<=. X0X0PLUSX1One = True 

    X1X0PLUSY0X1 .<=. X1X0PLUSY0X1 = True 
    X1X0PLUSY0X1 .<=. X0X1PLUSX1One = True 
    X1X0PLUSY0X1 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    X1X0PLUSY0X1 .<=. OneOne = True 

    X1X0PLUSY1X1 .<=. X1X0PLUSY1X1 = True 
    X1X0PLUSY1X1 .<=. X0X1PLUSX1One = True 
    X1X0PLUSY1X1 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    X1X0PLUSY1X1 .<=. OneOne = True 

    X1X0 .<=. X1One = True 
    X1X0 .<=. X1X0PLUSY0X1 = True 
    X1X0 .<=. X1X0PLUSY1X1 = True 
    X1X0 .<=. X1X0 = True 
    X1X0 .<=. X0Y1PLUSX1X0 = True 
    X1X0 .<=. X0Y1PLUSX1One = True 
    X1X0 .<=. X0Y0PLUSX1X0 = True 
    X1X0 .<=. X0Y0PLUSX1One = True 
    X1X0 .<=. X0X1PLUSX1X0 = True 
    X1X0 .<=. X0X1PLUSX1One = True 
    X1X0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X1X0 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    X1X0 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    X1X0 .<=. OneOne = True 
    X1X0 .<=. OneX0 = True 
    X1X0 .<=. X0X0PLUSX1One = True 

    X0Y1PLUSX1X0 .<=. X0Y1PLUSX1X0 = True 
    X0Y1PLUSX1X0 .<=. X0Y1PLUSX1One = True 
    X0Y1PLUSX1X0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0Y1PLUSX1X0 .<=. OneOne = True 

    X0Y1PLUSX1X1 .<=. X0Y1PLUSX1X1 = True 
    X0Y1PLUSX1X1 .<=. X0Y1PLUSX1One = True 
    X0Y1PLUSX1X1 .<=. OneOne = True 
    X0Y1PLUSX1X1 .<=. X0X0PLUSOneX1 = True 

    X0Y1PLUSX1Y0 .<=. X0Y1PLUSX1Y0 = True 
    X0Y1PLUSX1Y0 .<=. X0Y1PLUSX1One = True 
    X0Y1PLUSX1Y0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0Y1PLUSX1Y0 .<=. OneOne = True 

    X0Y1PLUSX1Y1PLUSY0Y0 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    X0Y1PLUSX1Y1PLUSY0Y0 .<=. OneOne = True 

    X0Y1PLUSX1Y1PLUSY1Y0 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    X0Y1PLUSX1Y1PLUSY1Y0 .<=. OneOne = True 

    OneY1 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    OneY1 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    OneY1 .<=. OneY1 = True 
    OneY1 .<=. X0Y1PLUSX1One = True 
    OneY1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    OneY1 .<=. OneOne = True 

    X0Y1PLUSX1One .<=. X0Y1PLUSX1One = True 
    X0Y1PLUSX1One .<=. OneOne = True 

    X0Y1PLUSY0Y0 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    X0Y1PLUSY0Y0 .<=. X0Y1PLUSY0Y0 = True 
    X0Y1PLUSY0Y0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0Y1PLUSY0Y0 .<=. OneOne = True 

    X0Y1PLUSY1Y0 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    X0Y1PLUSY1Y0 .<=. X0Y1PLUSY1Y0 = True 
    X0Y1PLUSY1Y0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0Y1PLUSY1Y0 .<=. OneOne = True 

    X0Y1 .<=. X0Y1PLUSX1X0 = True 
    X0Y1 .<=. X0Y1PLUSX1X1 = True 
    X0Y1 .<=. X0Y1PLUSX1Y0 = True 
    X0Y1 .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    X0Y1 .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    X0Y1 .<=. OneY1 = True 
    X0Y1 .<=. X0Y1PLUSX1One = True 
    X0Y1 .<=. X0Y1PLUSY0Y0 = True 
    X0Y1 .<=. X0Y1PLUSY1Y0 = True 
    X0Y1 .<=. X0Y1 = True 
    X0Y1 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0Y1 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0Y1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0Y1 .<=. X0One = True 
    X0Y1 .<=. OneOne = True 
    X0Y1 .<=. X0X0PLUSOneX1 = True 

    X0Y0PLUSX1X0 .<=. X0Y0PLUSX1X0 = True 
    X0Y0PLUSX1X0 .<=. X0Y0PLUSX1One = True 
    X0Y0PLUSX1X0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0Y0PLUSX1X0 .<=. OneOne = True 

    X0Y0PLUSX1X1 .<=. X0Y0PLUSX1X1 = True 
    X0Y0PLUSX1X1 .<=. X0Y0PLUSX1One = True 
    X0Y0PLUSX1X1 .<=. OneOne = True 
    X0Y0PLUSX1X1 .<=. X0X0PLUSOneX1 = True 

    X0Y0PLUSX1Y0PLUSY0Y1 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    X0Y0PLUSX1Y0PLUSY0Y1 .<=. OneOne = True 

    X0Y0PLUSX1Y0PLUSY1Y1 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    X0Y0PLUSX1Y0PLUSY1Y1 .<=. OneOne = True 

    OneY0 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    OneY0 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    OneY0 .<=. OneY0 = True 
    OneY0 .<=. X0Y0PLUSX1One = True 
    OneY0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    OneY0 .<=. OneOne = True 

    X0Y0PLUSX1Y1 .<=. X0Y0PLUSX1Y1 = True 
    X0Y0PLUSX1Y1 .<=. X0Y0PLUSX1One = True 
    X0Y0PLUSX1Y1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0Y0PLUSX1Y1 .<=. OneOne = True 

    X0Y0PLUSX1One .<=. X0Y0PLUSX1One = True 
    X0Y0PLUSX1One .<=. OneOne = True 

    X0Y0PLUSY0Y1 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    X0Y0PLUSY0Y1 .<=. X0Y0PLUSY0Y1 = True 
    X0Y0PLUSY0Y1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0Y0PLUSY0Y1 .<=. OneOne = True 

    X0Y0PLUSY1Y1 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    X0Y0PLUSY1Y1 .<=. X0Y0PLUSY1Y1 = True 
    X0Y0PLUSY1Y1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0Y0PLUSY1Y1 .<=. OneOne = True 

    X0Y0 .<=. X0Y0PLUSX1X0 = True 
    X0Y0 .<=. X0Y0PLUSX1X1 = True 
    X0Y0 .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    X0Y0 .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    X0Y0 .<=. OneY0 = True 
    X0Y0 .<=. X0Y0PLUSX1Y1 = True 
    X0Y0 .<=. X0Y0PLUSX1One = True 
    X0Y0 .<=. X0Y0PLUSY0Y1 = True 
    X0Y0 .<=. X0Y0PLUSY1Y1 = True 
    X0Y0 .<=. X0Y0 = True 
    X0Y0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0Y0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0Y0 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0Y0 .<=. X0One = True 
    X0Y0 .<=. OneOne = True 
    X0Y0 .<=. X0X0PLUSOneX1 = True 

    X0X1PLUSX1X0 .<=. X0X1PLUSX1X0 = True 
    X0X1PLUSX1X0 .<=. X0X1PLUSX1One = True 
    X0X1PLUSX1X0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0X1PLUSX1X0 .<=. OneOne = True 

    X0X1PLUSX1X1PLUSY0X0 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    X0X1PLUSX1X1PLUSY0X0 .<=. OneOne = True 

    X0X1PLUSX1X1PLUSY1X0 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    X0X1PLUSX1X1PLUSY1X0 .<=. OneOne = True 

    OneX1 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    OneX1 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    OneX1 .<=. OneX1 = True 
    OneX1 .<=. X0X1PLUSX1One = True 
    OneX1 .<=. OneOne = True 
    OneX1 .<=. X0X0PLUSOneX1 = True 

    X0X1PLUSX1Y0 .<=. X0X1PLUSX1Y0 = True 
    X0X1PLUSX1Y0 .<=. X0X1PLUSX1One = True 
    X0X1PLUSX1Y0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0X1PLUSX1Y0 .<=. OneOne = True 

    X0X1PLUSX1Y1 .<=. X0X1PLUSX1Y1 = True 
    X0X1PLUSX1Y1 .<=. X0X1PLUSX1One = True 
    X0X1PLUSX1Y1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0X1PLUSX1Y1 .<=. OneOne = True 

    X0X1PLUSX1One .<=. X0X1PLUSX1One = True 
    X0X1PLUSX1One .<=. OneOne = True 

    X0X1PLUSY0X0 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    X0X1PLUSY0X0 .<=. X0X1PLUSY0X0 = True 
    X0X1PLUSY0X0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0X1PLUSY0X0 .<=. OneOne = True 

    X0X1PLUSY1X0 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    X0X1PLUSY1X0 .<=. X0X1PLUSY1X0 = True 
    X0X1PLUSY1X0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0X1PLUSY1X0 .<=. OneOne = True 

    X0X1 .<=. X0X1PLUSX1X0 = True 
    X0X1 .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    X0X1 .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    X0X1 .<=. OneX1 = True 
    X0X1 .<=. X0X1PLUSX1Y0 = True 
    X0X1 .<=. X0X1PLUSX1Y1 = True 
    X0X1 .<=. X0X1PLUSX1One = True 
    X0X1 .<=. X0X1PLUSY0X0 = True 
    X0X1 .<=. X0X1PLUSY1X0 = True 
    X0X1 .<=. X0X1 = True 
    X0X1 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0X1 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0X1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0X1 .<=. X0One = True 
    X0X1 .<=. OneOne = True 
    X0X1 .<=. X0X0PLUSOneX1 = True 

    X0X0PLUSX0X1PLUSX1X0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0X0PLUSX0X1PLUSX1X0 .<=. OneOne = True 

    X0X0PLUSX0X1PLUSX1Y0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0X0PLUSX0X1PLUSX1Y0 .<=. OneOne = True 

    X0X0PLUSX0X1PLUSX1Y1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0X0PLUSX0X1PLUSX1Y1 .<=. OneOne = True 

    X0One .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0One .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0One .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0One .<=. X0One = True 
    X0One .<=. OneOne = True 
    X0One .<=. X0X0PLUSOneX1 = True 

    X0X0PLUSX1X0PLUSY0X1 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    X0X0PLUSX1X0PLUSY0X1 .<=. OneOne = True 

    X0X0PLUSX1X0PLUSY1X1 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    X0X0PLUSX1X0PLUSY1X1 .<=. OneOne = True 

    OneOne .<=. OneOne = True 

    OneX0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    OneX0 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    OneX0 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    OneX0 .<=. OneOne = True 
    OneX0 .<=. OneX0 = True 
    OneX0 .<=. X0X0PLUSX1One = True 

    X0X0PLUSX1X1 .<=. OneOne = True 
    X0X0PLUSX1X1 .<=. X0X0PLUSX1X1 = True 
    X0X0PLUSX1X1 .<=. X0X0PLUSX1One = True 
    X0X0PLUSX1X1 .<=. X0X0PLUSOneX1 = True 

    X0X0PLUSX1Y0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0X0PLUSX1Y0 .<=. OneOne = True 
    X0X0PLUSX1Y0 .<=. X0X0PLUSX1Y0 = True 
    X0X0PLUSX1Y0 .<=. X0X0PLUSX1One = True 

    X0X0PLUSX1Y1 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0X0PLUSX1Y1 .<=. OneOne = True 
    X0X0PLUSX1Y1 .<=. X0X0PLUSX1Y1 = True 
    X0X0PLUSX1Y1 .<=. X0X0PLUSX1One = True 

    X0X0PLUSX1One .<=. OneOne = True 
    X0X0PLUSX1One .<=. X0X0PLUSX1One = True 

    X0X0PLUSY0X1 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    X0X0PLUSY0X1 .<=. OneOne = True 
    X0X0PLUSY0X1 .<=. X0X0PLUSY0X1 = True 
    X0X0PLUSY0X1 .<=. X0X0PLUSOneX1 = True 

    X0X0PLUSY1X1 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    X0X0PLUSY1X1 .<=. OneOne = True 
    X0X0PLUSY1X1 .<=. X0X0PLUSY1X1 = True 
    X0X0PLUSY1X1 .<=. X0X0PLUSOneX1 = True 

    X0X0PLUSOneX1 .<=. OneOne = True 
    X0X0PLUSOneX1 .<=. X0X0PLUSOneX1 = True 

    X0X0 .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    X0X0 .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    X0X0 .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    X0X0 .<=. X0One = True 
    X0X0 .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    X0X0 .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    X0X0 .<=. OneOne = True 
    X0X0 .<=. OneX0 = True 
    X0X0 .<=. X0X0PLUSX1X1 = True 
    X0X0 .<=. X0X0PLUSX1Y0 = True 
    X0X0 .<=. X0X0PLUSX1Y1 = True 
    X0X0 .<=. X0X0PLUSX1One = True 
    X0X0 .<=. X0X0PLUSY0X1 = True 
    X0X0 .<=. X0X0PLUSY1X1 = True 
    X0X0 .<=. X0X0PLUSOneX1 = True 
    X0X0 .<=. X0X0 = True 

    ZeroZero .<=. Y1Y1 = True 
    ZeroZero .<=. Y1Y0 = True 
    ZeroZero .<=. Y1X1 = True 
    ZeroZero .<=. Y1One = True 
    ZeroZero .<=. Y1X0 = True 
    ZeroZero .<=. Y0Y1PLUSY1X0 = True 
    ZeroZero .<=. Y0Y1PLUSY1X1 = True 
    ZeroZero .<=. Y0Y1PLUSY1Y0 = True 
    ZeroZero .<=. Y0Y1 = True 
    ZeroZero .<=. Y0Y0PLUSY1X0 = True 
    ZeroZero .<=. Y0Y0PLUSY1X1 = True 
    ZeroZero .<=. Y0Y0PLUSY1Y1 = True 
    ZeroZero .<=. Y0Y0 = True 
    ZeroZero .<=. Y0X1PLUSY1X0 = True 
    ZeroZero .<=. Y0X1PLUSY1Y0 = True 
    ZeroZero .<=. Y0X1PLUSY1Y1 = True 
    ZeroZero .<=. Y0X1 = True 
    ZeroZero .<=. Y0One = True 
    ZeroZero .<=. Y0X0PLUSY1X1 = True 
    ZeroZero .<=. Y0X0PLUSY1Y0 = True 
    ZeroZero .<=. Y0X0PLUSY1Y1 = True 
    ZeroZero .<=. Y0X0 = True 
    ZeroZero .<=. X1Y1PLUSY0Y0 = True 
    ZeroZero .<=. X1Y1PLUSY1Y0 = True 
    ZeroZero .<=. X1Y1 = True 
    ZeroZero .<=. X1Y0PLUSY0Y1 = True 
    ZeroZero .<=. X1Y0PLUSY1Y1 = True 
    ZeroZero .<=. X1Y0 = True 
    ZeroZero .<=. X1X1PLUSY0X0 = True 
    ZeroZero .<=. X1X1PLUSY1X0 = True 
    ZeroZero .<=. X1X1 = True 
    ZeroZero .<=. X1One = True 
    ZeroZero .<=. X1X0PLUSY0X1 = True 
    ZeroZero .<=. X1X0PLUSY1X1 = True 
    ZeroZero .<=. X1X0 = True 
    ZeroZero .<=. X0Y1PLUSX1X0 = True 
    ZeroZero .<=. X0Y1PLUSX1X1 = True 
    ZeroZero .<=. X0Y1PLUSX1Y0 = True 
    ZeroZero .<=. X0Y1PLUSX1Y1PLUSY0Y0 = True 
    ZeroZero .<=. X0Y1PLUSX1Y1PLUSY1Y0 = True 
    ZeroZero .<=. OneY1 = True 
    ZeroZero .<=. X0Y1PLUSX1One = True 
    ZeroZero .<=. X0Y1PLUSY0Y0 = True 
    ZeroZero .<=. X0Y1PLUSY1Y0 = True 
    ZeroZero .<=. X0Y1 = True 
    ZeroZero .<=. X0Y0PLUSX1X0 = True 
    ZeroZero .<=. X0Y0PLUSX1X1 = True 
    ZeroZero .<=. X0Y0PLUSX1Y0PLUSY0Y1 = True 
    ZeroZero .<=. X0Y0PLUSX1Y0PLUSY1Y1 = True 
    ZeroZero .<=. OneY0 = True 
    ZeroZero .<=. X0Y0PLUSX1Y1 = True 
    ZeroZero .<=. X0Y0PLUSX1One = True 
    ZeroZero .<=. X0Y0PLUSY0Y1 = True 
    ZeroZero .<=. X0Y0PLUSY1Y1 = True 
    ZeroZero .<=. X0Y0 = True 
    ZeroZero .<=. X0X1PLUSX1X0 = True 
    ZeroZero .<=. X0X1PLUSX1X1PLUSY0X0 = True 
    ZeroZero .<=. X0X1PLUSX1X1PLUSY1X0 = True 
    ZeroZero .<=. OneX1 = True 
    ZeroZero .<=. X0X1PLUSX1Y0 = True 
    ZeroZero .<=. X0X1PLUSX1Y1 = True 
    ZeroZero .<=. X0X1PLUSX1One = True 
    ZeroZero .<=. X0X1PLUSY0X0 = True 
    ZeroZero .<=. X0X1PLUSY1X0 = True 
    ZeroZero .<=. X0X1 = True 
    ZeroZero .<=. X0X0PLUSX0X1PLUSX1X0 = True 
    ZeroZero .<=. X0X0PLUSX0X1PLUSX1Y0 = True 
    ZeroZero .<=. X0X0PLUSX0X1PLUSX1Y1 = True 
    ZeroZero .<=. X0One = True 
    ZeroZero .<=. X0X0PLUSX1X0PLUSY0X1 = True 
    ZeroZero .<=. X0X0PLUSX1X0PLUSY1X1 = True 
    ZeroZero .<=. OneOne = True 
    ZeroZero .<=. OneX0 = True 
    ZeroZero .<=. X0X0PLUSX1X1 = True 
    ZeroZero .<=. X0X0PLUSX1Y0 = True 
    ZeroZero .<=. X0X0PLUSX1Y1 = True 
    ZeroZero .<=. X0X0PLUSX1One = True 
    ZeroZero .<=. X0X0PLUSY0X1 = True 
    ZeroZero .<=. X0X0PLUSY1X1 = True 
    ZeroZero .<=. X0X0PLUSOneX1 = True 
    ZeroZero .<=. X0X0 = True 
    ZeroZero .<=. ZeroZero = True 

    _ .<=. _ = False


