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
            qlc =  boxAtomicQuestions qla qlb
            n = length qlc
            m = 300
            questions = take m qlc

            bwo = boxAtomicProduct qla qla
            pairs = [(a, b) | a <- questions, b <- questions]

            packed = packList questions
            -- packedRel = transitiveClosure $ relationFromFunction m $ packRel packed $ boxPrec (qla, qlb)
            preorder = preOrder questions $ boxPrec (qla, qlb)
            (Poset boxElems boxLess) = quotientLogic (PrePoset questions preorder) 
        putStrLn $ show $ n 
        -- putStrLn $ show $ packedRel
        -- putStrLn $ show $ V.length $ V.filter id $ Repa.toUnboxed closed 
        putStrLn $ show $ length boxElems
        -- putStrLn $ show $ V.length $ V.filter id $ Repa.toUnboxed packedRel 
