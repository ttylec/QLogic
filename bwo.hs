import QLogic
import QLogic.BoxWorld
import QLogic.BoxProduct

import Data.List

-- orthoCond :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Bool
-- orthoCond a b = a /\ b == Just (zero <> zero) && a \/ b == Just (one <> one)
-- 
-- orthoCandidates q = filter (\p -> (q <+> p) == (BoxProd one one))  $ filter (isBoxOrthogonal q) elements

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
        let a = X0 <> Y0
            b = X1 <> Y1
            c = a <+> b
            bwo_elements = (elements :: [BoxProduct Lattice4 Lattice4])

        -- putStrLn $ exportPosetStructure bwo_elements 

        -- putStrLn $ unlines $ map (mathematicaForm' . boxToRepr) bwo_elements
        
        putStrLn $ show bwo_elements
        putStrLn $ unlines $ map 
            (\a -> "ortho " ++ (show a) ++ " = " ++ (show $ ortho a)) --(show $ orthoCandidates a)) 
            bwo_elements

        putStrLn "Now let's check if our proposal of orthocompletion is idempotent"
        putStrLn $ if all (\p -> (ortho . ortho $ p) == p) bwo_elements then "Yes!" else "No..."

        putStrLn "Check order reverse"
        putStrLn $ if checkOrderReverse bwo_elements then "Yes!" else "No..."

        -- putStrLn $ show $ filter (\ (a, b) -> not $ (ortho a) .<. (ortho b)) $
        --     [(a, b) | a <- bwo_elements, b <- bwo_elements, a .>. b]
        -- putStrLn "Orthomodularity check"
        -- putStrLn $ if checkOrthomodular bwo_elements then "Yes!" else "No..."
        -- putStrLn $ show (elements :: [BoxProduct Lattice4 Lattice4])
        -- putStrLn $ show (elements :: [BoxProduct Lattice4 Lattice4])
        -- putStrLn $ show (elements :: [BoxProduct Lattice4 Lattice4])
