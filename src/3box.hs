{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Main (main) where

import System.Environment

import QLogic
import QLogic.GeneralBoxes
import QLogic.BoxWorld

main :: IO ()
main = do
  let
      constr = boxConstraints threeboxes
      boxWorld = boxWorldLogic threeboxes
      threeBoxElems = filter ((<= 1) . maxStateValue constr)
                      . elementsOf $ boxWorld

  (n:_) <- getArgs
  print n
  print $ length . elementsOf $ boxWorld
  -- print $ take (read n) . elementsOf $ boxWorld
  -- -- print $ take (read n) . elementsOf . logicRepr $ boxWorld
  print $ length threeBoxElems
