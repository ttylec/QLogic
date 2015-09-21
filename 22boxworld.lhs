> import Data.QLogic
> import Data.QLogic.BoxWorld
> import Data.QLogic.States

Firstly we define observables for boxes:

> x = Observable 'X' [0, 1]
> y = Observable 'Y' [0, 1]

Our box world is two box system,
where each box has two two-valued
observables:

> system = Two ([x, y], [x, y])

Them the logic is a concrete logic
given by:

> logic = boxWorldLogic system

PR-box state:
   XX  XY  YX  YY
00 1/2 1/2 1/2 0
01 0   0   0   1/2
10 0   0   0   1/2
11 1/2 1/2 1/2 0

> statePR = fromAtomicList logic $ [ (Question (Two (Atomic 'X' 0, Atomic 'X' 0)), 0.5)
>                            , (Question (Two (Atomic 'X' 0, Atomic 'Y' 0)), 0.5)
>                            , (Question (Two (Atomic 'Y' 0, Atomic 'X' 0)), 0.5)
>                            , (Question (Two (Atomic 'Y' 0, Atomic 'Y' 1)), 0.5)
>                            , (Question (Two (Atomic 'Y' 1, Atomic 'Y' 0)), 0.5)
>                            , (Question (Two (Atomic 'X' 1, Atomic 'X' 1)), 0.5)
>                            , (Question (Two (Atomic 'X' 1, Atomic 'Y' 1)), 0.5)
>                            , (Question (Two (Atomic 'Y' 1, Atomic 'X' 1)), 0.5) ] -- :: State (Question (Two Atomic)) Double

statePR = readState2 "[X0X0]=0.5, [X0Y0]=0.5, [Y0X0]=0.5, [Y0Y1]=0.5, [Y1Y0]=0.5, [X1X1]=0.5, [X1Y1]=0.5, [Y1X1]=0.5" logic toRepr

Let us check properties of the logic:



> main = do
>   print $ isLogic logic
>   print $ isBoolean logic
>   print $ isLattice logic
>   print $ length . twoValuedStates $ logicRepr logic   
