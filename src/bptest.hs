module Main where

import Data.Poset
import Data.QLogic
import Data.QLogic.Examples
import Data.QLogic.BoxProduct

main :: IO ()
main = do
        let qla = {-# SCC logics #-} booleanLogic [0..2] -- lanternLogic
            qlb = {-# SCC logics #-} booleanLogic [0..2] -- lanternLogic
            -- qlb = lanternLogic
            questions = boxQuestions qla qlb
            poset = boxAtomicProductPoset qla qlb
            poset2 = bpViaEquiv qla qlb

        putStrLn $ show $ length $ elementsOf poset2
