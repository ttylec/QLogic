import QLogic
import Poset
import QLogic.Examples.Lattice4
import QLogic.TwoTwoBoxWorld
import qualified Data.Vector as V
import Data.List

main :: IO ()
main = do
        let ela = elements :: [Lattice4]
            elb = atoms :: [TwoTwoBoxWorld]
            pairs = [a <> b | a <- ela, b <- elb, a /= zero, b /= zero]
            boxes@(BoxProduct (boxPropositions, _)) = boxProduct ela ela
            boxElems = V.fromList $ boxQuestions ela ela
            boxElems1 = nub $ makeAdmissibleSums pairs pairs
            -- boxPreRel = relationFromFunction boxElems boxLess
            -- boxRel = transitiveClosure boxPreRel
            -- (boxProp, boxPRel) = quotientSet (boxElems, boxRel)
            
        V.mapM_ (\a -> let b = reprOf a in putStrLn $ show $ (b, boxOrtho boxes b)) boxPropositions
        -- putStrLn $ show $ V.length boxElems
        -- putStrLn $ show boxProp
        -- putStrLn $ show $ V.length boxProp
