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
            packedRel = relationFromFunctionP m $ packRel packed $ boxPrec (qla, qlb)
            -- packed2 = packList q2
            -- ppo1 = transitiveClosure $ relationFromFunction n1 $ packRel packed1 $ boxPrec (qla, qla)
            -- ppo2 = transitiveClosure $ relationFromFunction n2 $ packRel packed2 $ POrd.boxPrec (qla, qla)
            -- o1 = unpackRel packed1 (relLess ppo1)
            -- o2 = unpackRel packed2 (relLess ppo2)
            -- pr1 = relationFromFunction n1 $ packRel packed1 $ boxPrec (qla, qla)
            -- pr2 = relationFromFunction n2 $ packRel packed2 $ POrd.boxPrec (qla, qla)
            -- pairs1 = [(a, b) | a <- q1, b <- q1]
            -- pairs2 = [(a, b) | a <- q2, b <- q2]
            -- pairLess1 = uncurry $ boxPrec (qla, qla)
            -- pairLess2 = uncurry $ POrd.boxPrec (qla, qla)
        putStrLn $ show $ length questions
        -- putStrLn $ show $ packedRel
        putStrLn $ show $ V.length $ V.filter id $ Repa.toUnboxed packedRel 
        -- putStrLn $ show $ packedRel
        -- putStrLn $ unlines $ map show $ zip q1 q2
        -- putStrLn $ unlines $ map show $ filter (\(x, y) -> pairLess1 x /= pairLess2 y) $ zip pairs1 pairs2
        -- putStrLn $ show $ o1 (X1<>X1) (X0<>One)
        -- putStrLn $ show $ o2 (X1 POrd.<> X1) (X0 POrd.<> One)
        -- putStrLn $ show $ elements $ bwo
        -- putStrLn $ show $ elementsOf bwo
        -- putStrLn $ show $ bwo
        -- putStrLn $ show $ boxPlus bwo (zeroOf bwo) (oneOf bwo)
        -- putStrLn $ show $ equivRepr $ zeroOf bwo
        -- putStrLn $ show $ equivRepr $ oneOf bwo
