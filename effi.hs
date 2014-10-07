import Poset

main :: IO ()
main = do
        let r = checkOrthomodular $ (elements :: [Product Space4 Space4])
        putStrLn $ if r then "Yes!\n" else "No!\n"
