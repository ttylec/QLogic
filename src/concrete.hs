import Data.QLogic.BoxWorld
import Data.QLogic

-- x = Observable "X" [0, 1]
-- y = Observable "Y" [0, 1]
-- left = [x, y]
-- right = [x, y]

x = Observable "X" [0, 1, 2]
y = Observable "Y" [0, 1, 2]
z = Observable "Z" [0, 1]

left = [x, y]
right = [x, y]

main :: IO ()
main = do
    let (set2q, q2set, ql) = boxWorldLogic2 left right
        a = elementsOf ql !! 10
        b = elementsOf ql !! 56
    
    putStrLn $ show . length $ elementsOf ql
    print $ set2q a
    print $ set2q b
    print $ orthoIn ql a b
    print $ orthoIn ql a $ ocmplIn ql a
    print $ compatIn ql a b
    let aa = readTwoBoxQuestion [x, y] "[X1Y0]+[X0X1]+[X0X0]"
        bb = readTwoBoxQuestion [x, y] "[X1Y0]+[X0Y1]+[X0Y0]" 
    print $ q2set aa == q2set bb
