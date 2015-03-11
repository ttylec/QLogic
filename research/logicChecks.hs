module Main where

import Data.Maybe

import Data.Poset
import Data.QLogic
import Data.QLogic.Examples
import Data.QLogic.BoxProduct
import Data.QLogic.Utils


checkCancelation :: (Ord a, Ord b) => (QLogic p1 a, QLogic p2 b) -> Poset (FreeProduct a b) -> Bool 
checkCancelation qls poset = and [claw p a b | p <- els, a <- els, b <- els, 
                                                     p `ortho` a, p `ortho` b]
    where
        -- claw :: BoxProduct a b -> BoxProduct a b -> BoxProduct a b -> Bool
        claw p a b = ((p @+@ a) ≤ (p @+@ b)) `iff` (a ≤ b) 
        els = elementsOf poset
        (≤) = lessIn poset
        (@+@) a b = fromJust $ freePlus qls poset a b
        ortho = freeDisj qls

main :: IO ()
main = do
        let questions = boxQuestions qla qlb
            -- qla = evenSubsets 4 -- lanternLogic
            -- qlb = evenSubsets 6 -- lanternLogic
            -- qla = lanternLogic
            qla = threeBoxLogic
            qlb = threeBoxLogic
            -- qlb = lanternLogic
            atomic_questions = boxAtomicQuestions qla qlb
            poset = boxAtomicProductPoset qla qlb
            poset2 = bpViaEquiv qla qlb
            preposet = boxPreAtomicProduct qla qlb

        putStrLn $ show $ length $ atomic_questions 
        putStrLn $ show $ length $ elementsOf preposet
        putStrLn $ show $ checkCancelation (qla, qlb) preposet
