import QLogic
-- import QLogic.Examples
import QLogic.BoxProduct
import QLogic.Examples.Lattice4
import QLogic.TwoTwoBoxWorld


main :: IO ()
main = do
        -- let bwo_elements = (elements :: [BoxProduct TwoTwoBoxWorld Lattice4])
        -- let bwo_elements = (elements :: [TwoTwoBoxWorld])
        let pairs = [a <> b | a <- (atoms :: [TwoTwoBoxWorld]), b <- (atoms :: [Lattice4])]
            ats = atoms :: [BoxProduct TwoTwoBoxWorld Lattice4]
            step1 = extendByList pairs ats

        putStrLn $ show $ length step1
