{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Main (main) where

import Control.Arrow
import Data.List
import Data.Tuple
import Data.Maybe
import qualified Data.Map as Map

import QLogic.GeneralBoxes
import QLogic.BoxWorld


main :: IO ()
main = do
  --
  -- Localized elements that are not Mackey-compatible
  -- (Ref. Dvurecenskij, Pulmannova, New Trends... p. 63 for def)
  --
  let p1 = read "[X0X0Y0]"
      p2 = read "[X0X0Y1]"
      p = p1 .@. p2
      q1 = read "[X1Y1Y0]"
      q2 = read "[X1Y1Y1]"
      q  = q1 .@. q2
      rr = read "[X0X1Y0]+[X1Y0Y0]"
      pp = p2 .@. q2
      -- r1 = read "[X0X0Y0]"
      -- rr = read "[X0X1Y0]+[X1X0Y0]+[X1X1Y0]" :: Proposition Box3
      -- rr' = read "[X0Y1Y0]+[X1Y0Y0]+[X1Y1Y0]" :: Proposition Box3
      r1 = read "[X0X0Y0]+[X1Y1Y0]"
      r = r1 .@. rr
      zz = p1 .@. q1

  putStr "p and q summable: "
  print $ summable p q
  putStr "rr, p2, q2 summable: "
  print $ summable (p2 .@. q2) rr
  putStr "rr + zz equal to r: "
  print $ equalP (rr .@. zz) $ read "[X0X0Y0]+[X0X1Y0]+[X1X0Y0]+[X1X1Y0]"
  putStr "p + q == pp + zz: "
  print $ equalP (p .@. q) (pp .@. zz)
  putStr "pp rr zz summable: "
  print $ summable pp (rr .@. zz)

  putStr "Consistency: "
  print $ all (not . uncurry summable) $ zip box3Atoms box3Atoms

  -- let p1 = read "[X0X0Y0]+[X0X1Y0]"
  --     pp = read "[X0X0Y1]+[X0X1Y1]"
  --     p = p1 .@. pp
  --     q1 = read "[X0X0Y0]+[X0X1Y0]"
  --     qq = read "[X1X0Y0]+[X1X1Y0]"

  -- putStr "p1 and pp summable: "
  -- print $ summable p1 pp
  -- putStr "q1 and pp summable: "
  -- print $ summable q1 qq
  -- putStr "qq and pp summable: "
  -- print $ summable pp qq

  -- Various dev-tests
  ---
  -- print $ maxStateValue gyni
  -- let p = head box3Atoms
  --     sump p = filter (summable p) box3Atoms
  -- print $ length box3Atoms
  -- print $ length box3varsMap
  -- print box3NoSignalingRank
  -- let q1 = read "[X0X0X0]+[X0X0X1]" :: Proposition Box3
  --     q2 = read "[X0X0Y0]+[X0X0Y1]" :: Proposition Box3
  --     q3 = read "[X0Y0Y0]+[X0Y0Y1]" :: Proposition Box3
  --     x  = read "[X0X0X0]" :: Proposition Box3
  --     one = map read [ "[X0X0X0]", "[X0X0X1]", "[X0X1X0]", "[X0X1X1]"
  --                    , "[X1X0X0]", "[X1X0X1]", "[X1X1X0]", "[X1X1X1]"
  --                    ] :: [Proposition Box3]
  -- print $ equalP q1 q2
  -- print $ equalP q2 q3
  -- print $ summable x x
  -- -- let g1 = makeAllSums box3Atoms (take 8 box3Atoms)
  -- let g1 = makeAllSums box3Atoms box3Atoms
  --     g2 = makeAllSums g1 box3Atoms
  -- putStrLn $ unlines $ map show $ allSums (take 8 box3Atoms)
  -- putStrLn $ unlines $ map show $ allSums box3Atoms
  -- print $ length $ allSums (take 32 box3Atoms)
  -- print $ equalP <$> box3Atoms <*> box3Atoms
  -- print $ length g1
  -- print $ length g2
  -- print $ map (length . sump) box3Atoms
  -- print $ maxStateValue . foldl1' (.@.) $ one
  -- print $ map (summable (one !! 7)) $ scanl1 (.@.) one
  -- print $ map (`elem` box3Atoms) one
  -- print $ length $ allSums one
