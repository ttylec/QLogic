import QLogic
import QLogic.Examples.Lattice4
import QLogic.TwoTwoBoxWorld
import QLogic.BoxProduct

import Data.List

mathematicaForm' :: FreeProduct Lattice4 Lattice4 -> String
mathematicaForm' (FreeProd x y)  
        | x == one && y == one = "question[1]"
        | x == zero && y == zero = "question[0]"
        | otherwise = "question[{" ++ (show a) ++ ", " ++ (show b) ++ "}, {"
            ++ (show alpha) ++ ", " ++ (show beta) ++ "}]"
        where
            (a, alpha) = decode x
            (b, beta) = decode y
            decode X0 = (1, 1)
            decode X1 = (1, 2)
            decode Y0 = (2, 1)
            decode Y1 = (2, 2)

mathematicaForm' a@(FreePlus _ _) = "CirclePlus[" 
    ++ (intercalate ", " $ map mathematicaForm' $ freeToList a)
    ++ "]"

mathForm :: BoxProduct Lattice4 Lattice4 -> String
mathForm = mathematicaForm' . boxToRepr

getGreaterThanList :: BoxProduct Lattice4 Lattice4 -> String
getGreaterThanList a = "{" ++ (mathForm a) ++ ", {" 
    ++ (intercalate ", " $ map mathForm $ greaterThan a)  ++ "}, "
    ++ (mathForm $ ortho a ) ++ "}"

exportPosetStructure :: [BoxProduct Lattice4 Lattice4] -> String
exportPosetStructure a = "{" ++
    (intercalate ", " $ map getGreaterThanList a)
    ++ "}"

main :: IO ()
main = do
        let bwo = (elements :: [BoxProduct Lattice4 Lattice4])
            ats = (atoms :: [BoxProduct Lattice4 Lattice4])
            test_elements = (testElements :: [BoxProduct Lattice4 Lattice4])

        putStrLn $ createStaticBoxProduct "TestTwoTwoBoxWorld" ats bwo

        -- putStrLn $ unlines $ map (mathematicaForm' . boxToRepr) bwo_elements
        
        -- putStrLn $ show (elements :: [BoxProduct Lattice4 Lattice4])
        -- putStrLn $ unlines $ map 
        --     (\a -> "ortho " ++ (show a) ++ " = " ++ (show $ orthoIn bwo a)) --(show $ orthoCandidates a)) 
        --     bwo
            -- (elements :: [TwoTwoBoxWorld])
-- 
--         putStrLn "Now let's check if our proposal of orthocompletion is idempotent"
--         putStrLn $ if all (\p -> (ortho . ortho $ p) == p) bwo_elements then "Yes!" else "No..."
-- 
--         putStrLn "Check order reverse"
--         putStrLn $ if checkOrderReverse bwo_elements then "Yes!" else "No..."
-- 
--         putStrLn "Check supremums"
--         putStrLn $ if checkSupremum bwo_elements then "Yes!" else "No..."
-- 
--         putStrLn "Orthomodularity check"
--         putStrLn $ if checkOrthomodular bwo_elements then "Yes!" else "No..."
