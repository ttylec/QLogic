import Data.QLogic
import Data.QLogic.BoxProduct
import Data.QLogic.BoxWorld
import Data.QLogic.Examples
import Debug.Trace
import Data.LinearProgram.GLPK.Solver (glpTermOut)

x = Observable "X" [0, 1]
y = Observable "Y" [0, 1]
bwsolver = boxWorldConstraints [x, y] [x, y]
boxl = boxLogic [x, y]

main = do
        let qs = boxAtomicQuestions boxl boxl
            
        glpTermOut
        -- putStrLn $ show qs 
        -- are_questions <- sequence $ [isBWQuestion bwsolver a | a <- qs]
        -- putStrLn $ show are_questions
        rel <- sequence $ [isBWLess bwsolver a b | a <- qs, b <- qs]
        putStrLn $ show $ rel


