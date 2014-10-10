import QLogic
import QLogic.BoxWorld
import QLogic.BoxProduct


orthoCond :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Bool
orthoCond a b = a /\ b == Just (zero <> zero) && a \/ b == Just (one <> one)

orthoCandidates q = filter (\p -> (q <+> p) == (BoxProd one one))  $ filter (isBoxOrthogonal q) elements

main :: IO ()
main = do
        let a = X0 <> Y0
            b = X1 <> Y1
            c = a <+> b
            bwo_elements = (elements :: [BoxProduct Lattice4 Lattice4])

        putStrLn $ unlines $ map 
            (\a -> "ortho " ++ (show a) ++ " = " ++ (show $ ortho a)) --(show $ orthoCandidates a)) 
            bwo_elements

        putStrLn "Now let's check if our proposal of orthocompletion is idempotent"
        putStrLn $ if all (\p -> (ortho . ortho $ p) == p) bwo_elements then "Yes!" else "No..."

        putStrLn "Check order reverse"
        putStrLn $ if checkOrderReverse bwo_elements then "Yes!" else "No..."

        putStrLn $ show $ filter (\ (a, b) -> not $ (ortho a) .<. (ortho b)) $
            [(a, b) | a <- bwo_elements, b <- bwo_elements, a .>. b]
        -- putStrLn "Orthomodularity check"
        -- putStrLn $ if checkOrthomodular bwo_elements then "Yes!" else "No..."
        -- putStrLn $ show (elements :: [BoxProduct Lattice4 Lattice4])
        -- putStrLn $ show (elements :: [BoxProduct Lattice4 Lattice4])
        -- putStrLn $ show (elements :: [BoxProduct Lattice4 Lattice4])
