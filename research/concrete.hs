import Data.QLogic.BoxWorld
import Data.QLogic

import qualified Data.IntSet as IntSet
import Data.IntSet ((\\))

x = Observable 'X' [0, 1]
y = Observable 'Y' [0, 1]
left = [x, y]
right = [x, y]

-- x = Observable 'X' [0, 1, 2]
-- y = Observable 'Y' [0, 1, 2]
-- z = Observable 'Z' [0, 1, 2]
-- left = [x, y, z]
-- right = [x, y, z]

main :: IO ()
main = do
    let ql = boxWorldLogic $ Two (left, right)
        q2set = toRepr ql
        set2q = fromRepr ql
        logic = logicRepr ql
        a = elementsOf ql !! 10
        b = elementsOf ql !! 56
        seta = q2set a
        seto = oneOf logic
    
    putStrLn $ show . length $ elementsOf ql
    -- print $ set2q a
    -- print $ set2q b
    print a
    print $ set2q . q2set $ a
    print b
    print $ set2q . q2set $ b

    print $ orthoIn ql a b
    print "===================================="
    print $ q2set a
    print $ oneOf logic 
    print "----"
    print $ IntSet.difference (oneOf logic) (q2set a)
    print $ IntSet.difference (q2set a) (oneOf logic)
    -- print $ (oneOf logic) \\ (q2set a)
    -- print $ seto \\ seta
    -- print $ seta \\ seto
    print "----"
    print $ ocmplIn logic $ q2set a
    print "===================================="
    print $ ocmplIn ql a
    print $ orthoIn ql a $ ocmplIn ql a
    print $ compatIn ql a b
    let aa = read "[X1Y0]+[X0X1]+[X0X0]" :: Question (Two Atomic)
        bb = read "[X1Y0]+[X0Y1]+[X0Y0]" :: Question (Two Atomic)
    print $ equalIn ql aa bb
