import Data.QLogic
import Data.QLogic.BoxProduct
import Data.QLogic.Examples
import Data.QLogic.Examples.TwoTwoBoxWorld
import qualified Data.Array.Repa as Repa
import qualified Data.Vector.Unboxed as V


main :: IO ()
main = do
        let qla = lanternLogic
            qlb = booleanLogic 2
            qlc = booleanLogic 3
            qlbp1 = boxAtomicProduct qla qla
            qlbp2 = boxAtomicProduct qlb qlc
            qlbp3 = boxAtomicProduct qlb qlbp2

        -- putStrLn $ "Check if qlbp1 is quantum logic: " ++ (show $ checkLogic qlbp1)
        -- putStrLn $ "Check if qlbp1 is boolean algebra: " ++ (show $ checkBoolean qlbp1)

        putStrLn $ show $ length $ elementsOf qlbp2
        -- putStrLn $ "Check if qlbp2 is quantum logic: " ++ (show $ checkLogic qlbp2)
        putStrLn $ "Check if qlbp2 is boolean algebra: " ++ (show $ checkBoolean qlbp2)
          
        -- putStrLn $ show $ length $ elementsOf qlbp3
        -- putStrLn $ "Check if qlbp3 is quantum logic: " ++ (show $ checkLogic qlbp3)
        -- putStrLn $ "Check if qlbp3 is boolean algebra: " ++ (show $ checkBoolean qlbp3)
