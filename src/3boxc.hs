module Main (main) where

import System.Environment

import QLogic
import QLogic.GeneralBoxes
import QLogic.BoxWorld

main :: IO ()
main = do
  let
      constr = boxConstraints threeboxes
      boxWorld = boxWorldLogic' threeboxes
      threeBoxElems = filter ((<= 1) . maxStateValue constr)
                      . elementsOf $ boxWorld

  -- (n:_) <- getArgs
  -- print n
  -- print $ length . elementsOf $ boxWorld

  let
    gyni1 = read "[X0X0X0]" ::  Question (Three Box)
    gyni2 = read "[X1Y1Y0]" ::  Question (Three Box)
    gyni3 = read "[Y0X1Y1]" ::  Question (Three Box)
    gyni4 = read "[Y1Y0X1]" ::  Question (Three Box)

  print $ summable constr gyni1 gyni2
  print $ summable constr (gyni1 .@. gyni2) gyni3
  print $ summable constr (gyni1 .@. gyni2 .@. gyni3) gyni4

  let
    gyni123 = gyni1 .@. gyni2 .@. gyni3

  print $ strongDisjoint (toRepr boxWorld gyni123) (toRepr boxWorld gyni4)
  -- print $ take (read n) . elementsOf $ boxWorld
  -- -- print $ take (read n) . elementsOf . logicRepr $ boxWorld
  -- print $ length threeBoxElems
