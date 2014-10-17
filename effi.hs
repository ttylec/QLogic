import QLogic
-- import QLogic.Examples
import QLogic.BoxProduct
import QLogic.BoxWorld


main :: IO ()
main = do
        let bwo_elements = (elements :: [BoxProduct TwoTwoBoxWorld Lattice4])
        -- let bwo_elements = (elements :: [TwoTwoBoxWorld])

        putStrLn $ show $ length bwo_elements
