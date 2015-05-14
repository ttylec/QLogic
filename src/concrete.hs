import Data.QLogic.BoxWorld
import Data.QLogic

-- x = Observable 'X' [0, 1]
-- y = Observable 'Y' [0, 1]
-- left = [x, y]
-- right = [x, y]

x = Observable 'X' [0, 1, 2]
y = Observable 'Y' [0, 1, 2]
z = Observable 'Z' [0, 1, 2]
left = [x, y, z]
right = [x, y, z]

main :: IO ()
main = do
    let (q2set, set2q, ql) = boxWorldLogic $ Two (left, right)
        a = elementsOf ql !! 10
        b = elementsOf ql !! 56
    
    putStrLn $ show . length $ elementsOf ql
    print $ set2q a
    print $ set2q b
    print $ orthoIn ql a b
    print $ orthoIn ql a $ ocmplIn ql a
    print $ compatIn ql a b
    let aa = readQ2 "[X1Y0]+[X0X1]+[X0X0]"
        bb = readQ2 "[X1Y0]+[X0Y1]+[X0Y0]" 
    print $ q2set aa == q2set bb
