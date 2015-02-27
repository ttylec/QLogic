module Main where

import Data.List.Split

import Data.QLogic
import Data.QLogic.BoxProduct
import Data.QLogic.Examples
import Data.QLogic.Utils

import Debug.Trace

exists3 :: [(a, b, c)] -> (a -> b -> c -> Bool) -> IO () 
exists3 set pred = do
        let n = length set `div` 10
        mapM_ (exists3' pred) $ chunksOf n set

exists3' pred set = do
        let r = any (\(a, b, c) -> pred a b c) set

        putStrLn $ show $ r

evensubs = (filter (even . length)) . subsets


main :: IO ()
main = do
        let set = [0, 1, 2, 3, 4, 5]
            qla = concreteLogic set $ evensubs set
            -- qlb = concreteLogic set $ evensubs set
        -- let qla = booleanLogic [1, 2, 3] -- lanternLogic
        --     qlb = booleanLogic [1, 2, 3] -- lanternLogic
        -- let qla = lanternLogic
            -- qlb = lanternLogic
            poset = boxPreAtomicProduct qla qlb 
            prele = boxPrec (qla, qlb)
            a `prequiv` b = a `prele` b && b `prele` a
            perp = freeDisj (qla, qlb) 
            le = lessIn poset
            zero = head $ minimalIn poset $ elementsOf poset

        -- putStrLn "p ~≤ q and q ~~ r implies p ~≤ r. This one is False:"
        -- putStrLn $ show $ and [(p `prele` q && q `prequiv` r) `implies` (p `prele` r) |
        --                       p <- elementsOf poset, q <- elementsOf poset, r <- elementsOf poset]

        -- putStrLn "p ~~ q and p ~≤ r implies q ~≤ r:"
        -- putStrLn $ show $ and [(p `prele` r && p `prequiv` q) `implies` (q `prele` r) |
        --                       p <- elementsOf poset, q <- elementsOf poset, r <- elementsOf poset]

        -- putStrLn "p ~≤ q⊕r and p ⟂ q implies p ~≤ r:"
        -- putStrLn $ show $ and [((p `prele` (q <+> r)) && p `perp` q) `implies` (p `prele` r) |
        --                       p <- elementsOf poset, q <- elementsOf poset, r <- elementsOf poset,
        --                       q `perp` r]

        putStrLn $ show poset
        putStrLn "p ≤ q⊕r and p ⟂ q implies p ≤ r:"
        exists3 [(p, q, r) | 
                p <- elementsOf poset, q <- elementsOf poset, r <- elementsOf poset, 
                q `perp` r, q /= zero, r /= zero] $ \p q r -> (((p `le` (q <+> r)) && p `perp` q) `implies` (p `le` r)) 
        -- putStrLn $ show $ and [(((p `le` (q <+> r)) && p `perp` q) `implies` (p `le` r)) |
        --                       p <- elementsOf poset, q <- elementsOf poset, r <- elementsOf poset,
        --                       q `perp` r, q /= zero, r /= zero]

