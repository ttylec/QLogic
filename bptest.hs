import Data.QLogic
import Data.QLogic.BoxProduct
import Data.QLogic.Examples
import Data.QLogic.Examples.TwoTwoBoxWorld
import qualified Data.Array.Repa as Repa
import qualified Data.Vector.Unboxed as V


main :: IO ()
main = do
        let qla = lanternLogic
            qlb = twoTwoBoxWorldLogic
            questions = boxAtomicQuestions qla qlb
            bwo = boxAtomicProduct qla qla
            m = 1000
            pairs = [(a, b) | a <- take m questions, b <- take m questions]

            n = length questions
            packed = packList questions
            packedRel = relationFromFunction m $ packRel packed $ boxPrec (qla, qlb)
            closed = transitiveClosure packedRel
        putStrLn $ show $ length questions
        -- putStrLn $ show $ packedRel
        putStrLn $ show $ V.length $ V.filter id $ Repa.toUnboxed closed 
        -- putStrLn $ show $ V.length $ V.filter id $ Repa.toUnboxed packedRel 
