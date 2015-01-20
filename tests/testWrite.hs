module Main where

import Data.QLogic
import Data.QLogic.IO
import Data.QLogic.Examples
import Data.QLogic.BoxProduct

main = do
        putStrLn $ writePoset "WTest" $ boxProduct lanternLogic lanternLogic
