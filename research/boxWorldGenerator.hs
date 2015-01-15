import Data.QLogic
import Data.QLogic.BoxProduct
import Data.QLogic.BoxWorld
import Data.QLogic.Examples
import Debug.Trace
import Data.LinearProgram.GLPK.Solver (glpTermOut)

boxQ = [[X0, X1], [Y0, Y1]]
lps = boxWorldConstraints boxQ boxQ

-- TODO: we need either way to construct IO (Poset a) 
-- from (a -> a -> m Bool)
--
-- Idea: maybe go for ListRel? 
--
-- or at least for now, store the result of BoxWorld lp solver
-- in file and allow to load it later.

main = do
        let qs = tail $ boxAtomicQuestions lanternLogic lanternLogic
            
        glpTermOut
        rel <- sequence $ [isBWLess lps a b | a <- qs, b <- qs]
        putStrLn $ show $ rel


