import Data.QLogic
import Data.QLogic.Examples
import Criterion.Main

explicitCheck :: Int -> Bool
explicitCheck n = checkLogic $ booleanLogic [0..n]

packedCheck :: Int -> Bool
packedCheck n = checkLogic $ packQLogic $ booleanLogic [0..n]


main :: IO ()
main = defaultMain [
                   bgroup "booleanLogics" [ bench "explicit 5" $ nf explicitCheck 5
                                          , bench "explicit 7" $ nf explicitCheck 7
                                          , bench "packed 5" $ nf packedCheck 5
                                          , bench "packed 7" $ nf packedCheck 7]
                                          ]
