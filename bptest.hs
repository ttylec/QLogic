import Data.QLogic
-- import Data.QLogic.BoxProductPOrd
import Data.QLogic.BoxProduct
import Data.QLogic.Examples
-- import qualified Data.QLogic.BoxProductPOrd as POrd


main :: IO ()
main = do
        let qla = lanternLogic
            bwo = boxAtomicProduct qla qla
            -- q1 = boxQuestions qla qla
            -- q2 = POrd.boxQuestions qla qla
            -- n1 = length q1
            -- n2 = length q2
            -- packed1 = packList q1
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

        -- putStrLn $ unlines $ map show $ zip q1 q2
        -- putStrLn $ unlines $ map show $ filter (\(x, y) -> pairLess1 x /= pairLess2 y) $ zip pairs1 pairs2
        -- putStrLn $ show $ o1 (X1<>X1) (X0<>One)
        -- putStrLn $ show $ o2 (X1 POrd.<> X1) (X0 POrd.<> One)
        -- putStrLn $ show $ elements $ bwo
        putStrLn $ show $ elementsOf bwo
        -- putStrLn $ show $ bwo
        -- putStrLn $ show $ boxPlus bwo (zeroOf bwo) (oneOf bwo)
        -- putStrLn $ show $ equivRepr $ zeroOf bwo
        -- putStrLn $ show $ equivRepr $ oneOf bwo
