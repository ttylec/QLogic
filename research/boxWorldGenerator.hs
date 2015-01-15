import Data.QLogic
import Data.QLogic.BoxProduct
import Data.QLogic.BoxWorld
import Data.QLogic.Examples
import Debug.Trace
import Data.LinearProgram.GLPK.Solver (glpTermOut)

boxQ = [[X0, X1], [Y0, Y1]]
lps = boxWorldConstraints boxQ boxQ


main = do
        let qs = tail $ boxAtomicQuestions lanternLogic lanternLogic
            
        glpTermOut
        rel <- sequence $ [isBWLess lps a b | a <- qs, b <- qs]
        putStrLn $ show $ rel


