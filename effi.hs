import Control.DeepSeq
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
            step2 = extendByList pairs step1
            nstep1 = testElements' pairs atoms
            nstep2 = testElements' pairs nstep1

        putStrLn $ show $ length (step1)
        putStrLn $ show $ length (step2)
