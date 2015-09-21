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
system = Three (box, box, [x])
-- system = Two (box, box)
-- system = Two ((Two (box, box)), box)

main :: IO ()
main = do
    let ql = boxWorldLogic system
        -- a  = read "[X0X0X0]+[X1X0X0]+[X0X1Y0]+[X1X1Y0]" :: Question (Three Atomic)
        -- b  = read "[Y0X0X0]+[Y0X0X1]+[Y0X1X0]+[Y0X1X1]" :: Question (Three Atomic)
        -- c  = read "[X0X0X0]+[X1X0X0]"                   :: Question (Three Atomic)
        -- c' = Question (Three (Trivial, Atomic 'X' 0, Atomic 'X' 0))
        q2set = toRepr ql
        set2q = fromRepr ql
        logic = logicRepr ql

    -- print $ q2set a
    -- print $ q2set b
    -- print $ set2q . q2set $ b
    -- -- print "Number of 
    -- -- print $ sum . map (length . set2q) $ elementsOf ql
    -- print $ q2set c == q2set c'
    -- print $ compatIn logic (q2set a) (q2set b) 
    -- print $ compatIn ql a b
    print $ length . elementsOf $ ql
    print $ isLogic logic
