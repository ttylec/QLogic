import QLogic
import QLogic.Examples.Lattice8
import QLogic.TwoTwoBoxWorld
import QLogic.BoxProduct

import Data.List

class MathForm a where
        mathForm :: a -> String
        mathRawTuple :: a -> (Int, Int)

instance MathForm Lattice8 where
        mathRawTuple X0 = (1, 1)
        mathRawTuple X1 = (1, 2)
        mathRawTuple Y0 = (2, 1)
        mathRawTuple Y1 = (2, 2)
        mathRawTuple Z0 = (3, 1)
        mathRawTuple Z1 = (3, 2)
        mathForm One = "question[1]"
        mathForm Zero = "question[0]"
        mathForm x = "question[" ++ (show a) ++ ", " ++ (show alpha) ++ "]"
            where
                (a, alpha) = mathRawTuple x

instance (FiniteLogic a, FiniteLogic b, MathForm a, MathForm b) => MathForm (FreeProduct a b) where
        mathForm a@(FreeProd x y) 
            | x == one && y == one = "question[1]"
            | x == zero && y == zero = "question[0]"
            | otherwise = "question[{" ++ (show a) ++ ", " ++ (show b) ++ "}, {"
                ++ (show alpha) ++ ", " ++ (show beta) ++ "}]"
            where
                (a, alpha) = mathRawTuple x
                (b, beta) = mathRawTuple y

        mathForm a@(FreePlus _ _) = "CirclePlus[" 
            ++ (intercalate ", " $ map mathForm $ freeToList a)
            ++ "]"

instance (FiniteLogic a, FiniteLogic b, MathForm a, MathForm b) => MathForm (BoxProduct a b) where
        mathForm = mathForm . boxRepr

formatPosetStructure :: (FiniteLogic a, FiniteLogic b, MathForm a, MathForm b) => 
    [BoxProduct a b] -> BoxProduct a b -> String
formatPosetStructure set a = "{" ++ (mathForm a) ++ ", {" 
    ++ (intercalate ", " $ map mathForm $ greaterThanIn set a)  ++ "}, "
    ++ (mathForm $ orthoIn set a ) ++ "}"

exportPosetStructure :: (FiniteLogic a, FiniteLogic b, MathForm a, MathForm b) => [BoxProduct a b] -> String
exportPosetStructure set = "{" ++
    (intercalate ", " $ map (formatPosetStructure set) set)
    ++ "}"

main :: IO ()
main = do
        let els = (elements :: [BoxProduct Lattice8 Lattice8])
            ats = (atoms :: [BoxProduct Lattice8 Lattice8])
            -- bwo = (boxProductElements :: [BoxProduct Lattice4 Lattice4])

        putStrLn $ show $ length els 
        putStrLn $ show $ length ats 
        -- putStrLn $ exportPosetStructure els
        -- putStrLn $ createStaticBoxProduct "TestTwoTwoBoxWorld" ats bwo

        -- putStrLn $ "L2"
        -- putStrLn $ if checkOrderReverse els then "Yes!" else "No..."

        -- putStrLn $ "L3"
        -- putStrLn $ if checkOrthoIdempotence els then "Yes!" else "No..."

        -- putStrLn $ "L4"
        -- putStrLn $ if checkSupremum els then "Yes!" else "No..."

        -- putStrLn $ "L5"
        -- putStrLn $ if checkOrthomodular els then "Yes!" else "No..."
