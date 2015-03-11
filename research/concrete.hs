import Data.QLogic.BoxWorldConcrete
import Data.QLogic

xa = Observable "Xa" 3
xb = Observable "Xb" 3
ya = Observable "Ya" 3
yb = Observable "Yb" 3
za = Observable "Za" 3
zb = Observable "Zb" 3

left = [xa, ya]
right = [xb, yb]

main :: IO ()
main = do
    let (qrepr, ql) = boxWorldCLogic2 left right
    
    putStrLn $ show . length $ elementsOf ql    
