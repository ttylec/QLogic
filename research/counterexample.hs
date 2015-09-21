module Main where

import Data.List
import Data.Maybe

import Data.QLogic
import qualified Data.QLogic.Examples as L
import Data.QLogic.Examples.NonTrivial3
import Data.QLogic.BoxProduct
import Data.QLogic.Utils

-- qs = boxAtomicQuestions tripleLogic tripleLogic lanternLogic
logics = (tripleLogic, L.lanternLogic)
poset = boxPreAtomicProduct tripleLogic L.lanternLogic
one = foldl1' (<+>) [a <> b | a <- head $ atomicDecomposition tripleLogic One,
                              b <- head $ atomicDecomposition L.lanternLogic L.One]
zero = Zero <> L.Zero

-- logics = (L.lanternLogic, L.lanternLogic)
-- poset = boxPreAtomicProduct L.lanternLogic L.lanternLogic
-- one = foldl1' (<+>) [a <> b | a <- head $ atomicDecomposition L.lanternLogic L.One,
--                               b <- head $ atomicDecomposition L.lanternLogic L.One]
-- zero = L.Zero <> L.Zero

complement ql a = filter isGood . filter (freeDisj logics a) . elementsOf $ ql 
    where
        isGood b = case supIn ql a b of
            Just o  -> one === o
            otherwise -> False
        (===) = equalIn ql

p = X<>L.X0 <+> Z<>L.Y0 <+> X'<>L.X1
q = X<>L.X0 <+> Z<>L.X0 <+> X'<>L.X1

main :: IO ()
main = do
    print $ length . elementsOf $ poset 
    print $ filter (null . complement poset) $ elementsOf poset
    -- print $ complement poset $ p 
    -- print $ complement poset $ q 
    -- print $ equalIn poset p q
    -- print $ complement poset $ L.X0<>L.X0
    -- print $ complement poset $ L.X0<>L.X1 <+> L.X1<>L.X0 <+> L.X1<>L.X1
    

