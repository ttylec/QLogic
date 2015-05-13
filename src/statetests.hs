module Main where


import Data.QLogic
import Data.QLogic.States
import Data.QLogic.BoxWorld
import Data.QLogic.Examples

import Data.Maybe
import Data.Set (Set, union)
import System.Random

x = Observable "X" [0, 1]
y = Observable "Y" [0, 1]

x0 = AtomicQuestion x 0
x1 = AtomicQuestion x 1
y0 = AtomicQuestion y 0
y1 = AtomicQuestion y 1

left = [x, y]
right = [x, y]
phase = phaseSpace2 left right
bwo = boxWorldLogic2 left right
bwoaqs = boxWorldAtomicQs2 left right

rho1 :: Set TwoSystems -> Double
rho1 q
    | q == boxQRepr2 phase (x0 <> x0) = 0.5 
    | q == boxQRepr2 phase (x0 <> x1) = 0
    | q == boxQRepr2 phase (x1 <> x0) = 0
    | q == boxQRepr2 phase (x1 <> x1) = 0.5

    | q == boxQRepr2 phase (x0 <> y0) = 0.5
    | q == boxQRepr2 phase (x0 <> y1) = 0
    | q == boxQRepr2 phase (x1 <> y0) = 0
    | q == boxQRepr2 phase (x1 <> y1) = 0.5

    | q == boxQRepr2 phase (y0 <> x0) = 0.5
    | q == boxQRepr2 phase (y0 <> x1) = 0
    | q == boxQRepr2 phase (y1 <> x0) = 0
    | q == boxQRepr2 phase (y1 <> x1) = 0.5

    | q == boxQRepr2 phase (y0 <> y0) = 0
    | q == boxQRepr2 phase (y0 <> y1) = 0.5
    | q == boxQRepr2 phase (y1 <> y0) = 0.5
    | q == boxQRepr2 phase (y1 <> y1) = 0
    
state r = fromAtomicState $ AtomicState bwo r

xa = simulateMeasurement (mkStdGen 0) (state rho1) $ boxQRepr2 phase $ x1<>x0 <+> x1<>x1
ya = simulateMeasurement (mkStdGen 1234) (state rho1) $ boxQRepr2 phase $ y1<>x0 <+> y1<>x1
xb = simulateMeasurement (mkStdGen 5678) (state rho1) $ boxQRepr2 phase $ x0<>x1 <+> x1<>x1
yb = simulateMeasurement (mkStdGen 9) (state rho1) $ boxQRepr2 phase $ x0<>y1 <+> x1<>y1


prCorr :: (RandomGen g) => g -> (TwoBoxQuestion -> BWLElem) -> BWLogic -> State BWLElem Double 
       -> Observable -> Observable -> [Double]
prCorr g repr ql rho x y = map corr $ simulateMeasurement g rho $ unsafeSupIn ql q00 q11
      where
          q00 = repr $ Composite (AtomicQuestion x 0) (AtomicQuestion y 0)
          q11 = repr $ Composite (AtomicQuestion x 1) (AtomicQuestion y 1)
          corr True  = 1
          corr False = -1

cor :: Bool -> Bool -> Double
cor True True = 1
cor False False = 1
cor _ _ = -1

chsh :: (Bool, Bool) -> (Bool, Bool) -> Double
chsh (xa, ya) (xb, yb) = cor xa xb + cor xa yb + cor ya xb - cor ya yb

n = 1000

selist = filter (not . uncurry (disjointIn bwo)) $ filter (isJust . uncurry (supIn bwo)) 
            [(p, q) | p <- atomsOf bwo, q <- atomsOf bwo, p /= q]

main :: IO ()
main = do
    let clist = map (uncurry chsh) $ zip (zip xa ya) (zip xb yb)
    g <- getStdGen
-- CHSH checks 
--     putStrLn $ show . take 10 $ prCorr g (boxQRepr2 phase) bwo (state rho1) x x
--     putStrLn $ show . take 10 $ prCorr g (boxQRepr2 phase) bwo (state rho1) x y
--     putStrLn $ show . take 10 $ prCorr g (boxQRepr2 phase) bwo (state rho1) y x
--     putStrLn $ show . take 10 $ prCorr g (boxQRepr2 phase) bwo (state rho1) y y
--     putStrLn $ show . take 10 $ simulateMeasurement g (state rho1) $ boxQRepr2 phase (y0 <> y0) 
--     putStrLn $ show . take 10 $ simulateMeasurement g (state rho1) $ boxQRepr2 phase (y0 <> y1) 
--     putStrLn $ show . take 10 $ simulateMeasurement g (state rho1) $ boxQRepr2 phase (y1 <> y0) 
--     putStrLn $ show . take 10 $ simulateMeasurement g (state rho1) $ boxQRepr2 phase (y1 <> y1) 
   
    putStrLn $ show $ boxQRepr2 phase $ x0<>x1 <+> x1<>x0
    putStrLn $ show $ union (boxQRepr2 phase $ x0<>x1) (boxQRepr2 phase $ x1<>x0)

    putStrLn $ show $ length selist
    putStrLn $ show $ take 10 selist
    
    putStrLn $ show $ length $ filter (\(p, q) -> (p `union` q) /= unsafeSupIn bwo p q) $ selist
    putStrLn $ (\(p, q) -> show (p `union` q) ++ "\n" ++ (show $ unsafeSupIn bwo p q)) $ head selist
    putStrLn $ show $ (\(p, q) -> (atomFromRepr phase p bwoaqs,  atomFromRepr phase q bwoaqs)) $ head selist
    putStrLn $ show $ fromRepr bwo phase (uncurry (unsafeSupIn bwo) . head $ selist) bwoaqs
    -- State checks
    -- putStrLn $ show . (isStateI bwo) $ state rho1
    -- putStrLn $ show . (isStateII bwo) $ state rho1
    -- -- putStrLn $ show $ oneOf bwo
    -- putStrLn . unlines . map show $ atomicDecomposition bwo $ oneOf bwo
    -- putStrLn $ show $ evalState (state rho1) (oneOf bwo)


    -- putStrLn $ show $ take 10 $ simulateMeasurement (randoms g) (state rho1) $ boxQRepr2 phase (x0 <> x0) 
    -- putStrLn $ show $ take 10 $ simulateMeasurement g (state rho1) $ boxQRepr2 phase (x0 <> x0) 
