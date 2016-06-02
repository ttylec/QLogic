{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Main (main) where

import System.Environment

import QLogic
import QLogic.GeneralBoxes
import QLogic.BoxWorld

main :: IO ()
main = do
  let
      boxWorld1 = logicRepr $ boxWorldLogic' onebox
      boxWorld2 = logicRepr $ boxWorldLogic' twoboxes
      -- constr = boxConstraints threeboxes
      -- boxWorld = boxWorldLogic' threeboxes
      -- threeBoxElems = filter ((<= 1) . maxStateValue constr)
      --                 . elementsOf $ boxWorld

  -- (n:_) <- getArgs
  -- print n
  print $ elementsOf boxWorld1
  print $ elementsOf . cartesian $ two boxWorld1 boxWorld1
  print $ length . elementsOf $ boxWorld2
  -- print $ take (read n) . elementsOf $ boxWorld
  -- -- print $ take (read n) . elementsOf . logicRepr $ boxWorld
  -- print $ length threeBoxElems
