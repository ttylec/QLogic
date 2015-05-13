module Main (main) where

import Data.Ord
import Data.List
import Data.QLogic.BoxWorld
import Data.QLogic.Utils
import Data.QLogic

import qualified Data.Set as Set

x = Observable "X" [0, 1]
y = Observable "Y" [0, 1]

(repr, bwl) = boxWorldLogic2 [x, y] [x, y] 
blocks = reverse . sortBy (comparing length) . filter (mutuallyCompatibleIn bwl) . subsets . atomsOf $ bwl

prettyPrint = show . head . repr
prettyPrintList = show . map (head . repr)

reduce :: [[Set.Set TwoSystems]] -> [[Set.Set TwoSystems]]
reduce = map Set.toList . reduce' [] . map Set.fromList

reduce' accum [] = accum
reduce' accum (b:bs)
    | b `alreadyIn` accum = reduce' accum bs
    | otherwise = reduce' (b:accum) bs
      where
          a `alreadyIn` as = any (a `Set.isSubsetOf`) as

main :: IO ()
main = do
   putStrLn $ unlines $ map prettyPrintList $ reduce blocks
