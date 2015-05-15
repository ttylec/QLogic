import Data.QLogic.BoxWorld
import Data.QLogic

import Data.List
import Data.Poset.ConcretePoset
import Data.QLogic
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.QLogic.Utils
import Data.Maybe

x = Observable 'X' [0, 1]
y = Observable 'Y' [0, 1]

box = [x, y]
system = Three (box, box, box)
-- system = Two (box, box)
-- system = Two ((Two (box, box)), box)

main :: IO ()
main = do
    let (q2set, set2q, ql) = boxWorldLogic system
        a  = readQ3 "[X0X0X0]+[X1X0X0]+[X0X1Y0]+[X1X1Y0]"
        b  = readQ3 "[Y0X0X0]+[Y0X0X1]+[Y0X1X0]+[Y0X1X1]"
        c  = readQ3 "[X0X0X0]+[X1X0X0]"
        c' = Primitive (Three (Trivial, AtomicQuestion 'X' 0, AtomicQuestion 'X' 0))

    print $ q2set a
    print $ q2set b
    print $ set2q . q2set $ b
    -- print "Number of 
    -- print $ sum . map (length . set2q) $ elementsOf ql
    print $ q2set c == q2set c'
    print $ compatIn ql (q2set a) (q2set b) 
    print $ length . elementsOf $ ql
