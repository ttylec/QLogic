import QLogic
import QLogic.BoxWorld


orthoCond :: (Logic a, Logic b) => BoxProduct a b -> BoxProduct a b -> Bool
orthoCond a b = a /\ b == Just (zero <> zero) && a \/ b == Just (one <> one)

orthoCandidates q = filter (\p -> (q <+> p) == (BoxProd one one))  $ filter (isBoxOrthogonal q) elements

main :: IO ()
main = do
        let a = X0 <> Y0
            b = X1 <> Y1
            c = a <+> b
            bwo_elements = (elements :: [BoxProduct Lattice4 Lattice4])
-- ortho (X1X1)⊕(X0Y0) = (ZeroZero)
        
        -- putStrLn $ show $ filter (orthoCond c) bwo_elements
        -- putStrLn $ show $ lessThan c
        -- putStrLn $ show $ maximal $ filter (isBoxOrthogonal c) bwo_elements
        -- putStrLn $ show $ length bwo_elements
        -- putStrLn $ show $ (ortho a) /\ (ortho b)
        -- putStrLn $ show $ ortho (a <+> b)
        putStrLn $ unlines $ map 
            (\a -> "ortho " ++ (show a) ++ " = " ++ (show $ ortho a)) --(show $ orthoCandidates a)) 
            bwo_elements
        putStrLn "Now let's check if our proposal of orthocompletion is idempotent"
        putStrLn $ if all (\p -> (ortho . ortho $ p) == p) bwo_elements then "Yes!" else "No..."
        -- putStrLn $ show (elements :: [BoxProduct Lattice4 Lattice4])
        -- putStrLn $ show (elements :: [BoxProduct Lattice4 Lattice4])
        -- putStrLn $ show (elements :: [BoxProduct Lattice4 Lattice4])
