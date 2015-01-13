import Data.QLogic
import Data.QLogic.Examples
import Criterion.Main

explicitCheck :: Int -> Bool
explicitCheck n = checkLogic $ booleanLogic [0..n]

packedCheck :: Int -> Bool
packedCheck n = checkLogic $ packQLogic $ booleanLogic [0..n]


main :: IO ()
main = defaultMain [
                   bgroup "booleanLogics" [ bench "explicit 5" $ whnf explicitCheck 5
                                          , bench "explicit 7" $ whnf explicitCheck 7
                                          , bench "packed 5" $ whnf packedCheck 5
                                          , bench "packed 7" $ whnf packedCheck 7]
                                          ]
