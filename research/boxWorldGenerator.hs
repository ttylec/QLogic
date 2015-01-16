import Data.QLogic
import Data.QLogic.BoxProduct
import Data.QLogic.BoxWorld
import Data.QLogic.Examples
import Debug.Trace
import Data.LinearProgram.GLPK.Solver (glpTermOut)

boxQ = [[X0, X1], [Y0, Y1]]
lps = boxWorldConstraints (Zero <> Zero) boxQ boxQ

-- TODO: store the result of BoxWorld lp solver
-- in file and allow to load it later.

main = do
        let qs = boxAtomicQuestions lanternLogic lanternLogic
            bp_poset = boxProduct' lanternLogic lanternLogic qs
            
        glpTermOut
        poset <- fromFuncM qs $ isBWLess lps 
        putStrLn $ show $ poset
        putStrLn $ show $ bp_poset


