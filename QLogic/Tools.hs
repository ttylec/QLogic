module QLogic.Tools where

listCycleBy :: (a -> a -> Bool) -> [a] -> a
listCycleBy _ (x:[]) = x
listCycleBy eq xs =  firstCycle' [] xs
    where
        firstCycle' prefix (x:xs)
            | any (eq x) prefix = x
            | otherwise = firstCycle' (x:prefix) xs

firstCycleBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
firstCycleBy eq f x0 = listCycleBy eq $ iterate f x0

fixedPointListBy :: (a -> a -> Bool) -> (a -> a) -> a -> [a]
fixedPointListBy eq f x0 = takeUntilStable $ iterate f x0
    where
        takeUntilStable (x:ys@(y:_))
            | x `eq` y = [y]
            | otherwise = x:(takeUntilStable ys)

fixedPointBy :: (a -> a -> Bool) -> (a -> a) -> a -> a 
fixedPointBy eq f x0 = stable $ iterate f x0
    where
        stable (x:ys@(y:_))
            | x `eq` y = y
            | otherwise = stable ys

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint = fixedPointBy (==) 

