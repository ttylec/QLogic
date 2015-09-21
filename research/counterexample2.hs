module Main where

import Data.List
import Data.List.Split hiding (oneOf)
import Data.Maybe

import Data.QLogic
import Data.QLogic.Examples
import Data.QLogic.BoxProduct
import Data.QLogic.Utils

import Control.Monad

-- qs = boxAtomicQuestions tripleLogic tripleLogic lanternLogic
logic1 = evenSubsets 8
logic2 = lanternLogic
logics = (logic1, logic2)

qs = boxAtomicQuestions logic1 logic2
poset = boxPreAtomicProduct logic1 logic2

logic = poset
-- zero = L.Zero <> L.Zero

complement ql a = filter isGood . filter (freeDisj logics a) . elementsOf $ ql 
    where
        isGood b = case supIn ql a b of
            Just o  -> one === o
            otherwise -> False
        (===) = equalIn ql

complement' els a = filter isGood . filter (freeDisj logics a) $ els 
    where
        isGood b = case supIn logic a b of
            Just o  -> one === o
            otherwise -> False
        (===) = equalIn logic

complementProgress a = do
    let els = elementsOf $ logic
        n = length els
        k = 1000
        size = n `div` k
    print $ "Nb of elements: " ++ (show n)
    print $ "Size of chunk: " ++ (show size)
    res <- forM (zip [0..] $ chunksOf size els) $ \(i, chunk) -> do
        let r = complement' chunk a

        print $ (show $ i * size) ++ " of " ++ (show n) ++ " done. Found " 
                    ++ (show $ length r) ++ " candidates"
        return r
    return res

zero1 = zeroOf logic1
one = foldl1' (<+>) [a <> b | a <- head $ atomicDecomposition logic1 $ oneOf logic1,
                              b <- head $ atomicDecomposition logic2 One]

triples = filter ((1 <) . length . lubIn logic1) candidates
    where
       candidates = [[a, b, c] 
                    | a <- elementsOf logic1
                    , b <- elementsOf logic1
                    , c <- elementsOf logic1
                    , a `ortho` b, b `ortho` c, not (a `ortho` c)
                    , a /= b, b /= c, a /= c
                    , a /= zero1, b /= zero1, c /= zero1]
       ortho = orthoIn logic1

makeP (a:c:e:[]) = a<>X0 <+> c<>Y0 <+> e<>X1

p = makeP $ head triples

main :: IO ()
main = do
    print $ length qs
    -- print $ length . elementsOf $ poset
    print $ p
    -- print $ complement poset p  
    cmpl <- complementProgress p  
    print cmpl
    -- print $ filter ((1 <) . length  . lubIn logic1) [[a, b, c] | a <- elementsOf logic1
    --                                                            , b <- elementsOf logic1
    --                                                            , c <- elementsOf logic1]
    -- print $ length . elementsOf $ poset 
    -- print $ filter (null . complement poset) $ elementsOf poset
    -- print $ complement poset $ p 
    -- print $ complement poset $ q 
    -- print $ equalIn poset p q
    -- print $ complement poset $ L.X0<>L.X0
    -- print $ complement poset $ L.X0<>L.X1 <+> L.X1<>L.X0 <+> L.X1<>L.X1
    

