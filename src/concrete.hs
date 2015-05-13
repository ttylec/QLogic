import Data.QLogic.BoxWorld
import Data.QLogic

x = Observable "X" [0, 1, 2]
y = Observable "Y" [0, 1, 2]
z = Observable "Z" [0, 1, 2]

left = [x, y, z]
right = [x, y, z]

main :: IO ()
main = do
    let (repr, ql) = boxWorldLogic2 left right
    
    putStrLn $ show . length $ elementsOf ql
    putStrLn $ show . repr $ elementsOf ql !! 10
