import Data.QLogic
import Data.QLogic.BoxProduct
import Data.QLogic.BoxWorld
import Data.QLogic.Examples
import Data.QLogic.IO
import Debug.Trace
import Data.LinearProgram.GLPK.Solver (glpTermOut)

boxQ = [[X0, X1], [Y0, Y1]]
lps = boxWorldConstraints (Zero <> Zero) boxQ boxQ

-- For testing
x = Observable "X" [0, 1]
y = Observable "Y" [0, 1]
z = Observable "Z" [0, 1]
bbo = boxWorld [x, y] [x, y]
bbo_constr = hsBoxWorldConstraints bbo
box_logic = boxLogic [x, y] 

-- TODO: 
-- * store the result of BoxWorld lp solver
--   in file and allow to load it later.


main = do
        let qs = boxAtomicQuestions lanternLogic lanternLogic
            bp_poset = boxProduct' lanternLogic lanternLogic qs
            hs_qs = boxAtomicQuestions box_logic box_logic

        putStrLn $ writeQLogic "TwoTwoBW" $ box_logic  --boxAtomicProduct box_logic box_logic
        -- putStrLn $ printBounds bbo bbo_constr
        -- putStrLn $ show $ length hs_qs
        -- putStrLn $ show $ packPoset $ fromFunc hs_qs $ hsIsLess bbo bbo_constr
            
        -- putStrLn $ unlines $ map (\q -> (show q) ++ ": " ++ (show $ unsafePerformIO $ isBWQuestion lps q)) qs 
        -- poset <- fromFuncM qs $ isBWLess lps 
        -- putStrLn $ show $ poset
        -- putStrLn $ show $ bp_poset


