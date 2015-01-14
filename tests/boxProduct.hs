module Main where

import Data.Poset
import Data.QLogic
import Data.QLogic.Examples
import Data.QLogic.BoxProduct

main :: IO ()
main = do
        let qla = lanternLogic
            qlb = lanternLogic
            questions = boxQuestions qla qlb
            pre = boxProduct' qla qlb questions
            poset = boxAtomicProduct qla qlb

        putStrLn $ show $ poset
