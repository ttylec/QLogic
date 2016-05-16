{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Main (main) where

import Control.Arrow
import Data.List
import Data.Tuple
import Data.Maybe
import qualified Data.Map as Map

import QLogic.BoxWorld (System, Three(Three))

-- Linear programming
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.LPMonad
import Data.LinearProgram

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra

-- Parsing
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.ByteString.Char8 (pack, unpack)

data Box = Box Char Int deriving (Eq, Ord)
type Box3 = Three Box

-- instance Ord Box where
--   Box c i `compare` Box d j = (c `compare` d) `mappe`

instance Show Box where
  show (Box o i) = o : show i

infixr 4 :@:

data Proposition a = Atom a | a :@: (Proposition a) deriving (Ord, Eq)

instance Show a => Show (Proposition a) where
  show (Atom a) = show a
  show (a :@: b) = show a ++ "+" ++ show b

instance Foldable Proposition where
  foldMap f (Atom a) = f a
  foldMap f (a :@: b) = f a `mappend` foldMap f b

(.@.) :: Ord a => Proposition a -> Proposition a -> Proposition a
(Atom a) .@. (Atom b)        | a <= b    = a :@: Atom  b
                             | otherwise = b :@: Atom a
(Atom a) .@. p@(b :@: c)     | a <= b    = a :@: p
                             | otherwise = b :@: (Atom a .@. c)
(a :@: b) .@. p = Atom a .@. b .@. p

infixr 5 .@.

-- |All box questions on given input
boxes :: Char -> [Box]
boxes c = map (Box c) [0, 1]

-- |All atomic questions about a box
boxQs :: [Box]
boxQs = concat idDecomp1

-- |All atomic propositions on a three box system
box3Atoms :: [Proposition Box3]
box3Atoms = map Atom $ three <$> boxQs <*> boxQs <*> boxQs

-- |List of box questions that sum to one
-- in one box
idDecomp1 :: [[Box]]
idDecomp1 = [boxes 'X', boxes 'Y']
-- |in three boxes (thanks to Traversable Three!)
idDecomp3 :: [Three [Box]]
idDecomp3 = three <$> idDecomp1 <*> idDecomp1 <*> idDecomp1

gyni :: Proposition (Three Box)
gyni = foldl1' (.@.) $ map Atom gyniboxes

gyniboxes :: [Three Box]
gyniboxes = map Three [ (Box 'X' 0, Box 'X' 0, Box 'X' 0)
                      , (Box 'X' 1, Box 'Y' 1, Box 'Y' 0)
                      , (Box 'Y' 0, Box 'X' 1, Box 'Y' 1)
                      , (Box 'Y' 1, Box 'Y' 0, Box 'X' 1)
                      ]

toLinFunc :: Proposition Box3 -> LinFunc Box3 Int
toLinFunc (Atom a) = var a
toLinFunc (a :@: b) = var a ^+^ toLinFunc b

boxConstraints :: LPM Box3 Int ()
boxConstraints =
  do
    mapM_ (uncurry equal) nonsignaling
    mapM_ (`equalTo` 1) normalization
    mapM_ (`constrain` Bound 0 1) box3vars

glpkSolverOpts :: GLPOpts
glpkSolverOpts = MipOpts MsgOff 10000 True DrTom LocBound AllPre False [] 0.0

-- |Helper function since Three requires a triple
three :: a -> a -> a -> Three a
three a b c = Three (a, b, c)

normalization :: [LinFunc Box3 Int]
normalization = map (varSum . sequenceA) idDecomp3

nonsignaling' :: Box -> Box -> ([Box3], [Box3])
nonsignaling' p q = (lhs, rhs)
  where
    lhs = map (three p q) $ boxes 'X'
    rhs = map (three p q) $ boxes 'Y'

nonsignaling :: [(LinFunc Box3 Int, LinFunc Box3 Int)]
nonsignaling = map (varSum *** varSum) $ concat [nsone, nstwo, nsthree]
  where
    nsone = map (uncurry nonsignaling') pqs
    nstwo = map (mapShiftBox . uncurry nonsignaling') pqs
    nsthree = map (mapShiftBox . mapShiftBox . uncurry nonsignaling') pqs
    pqs = (,) <$> boxQs <*> boxQs
    mapShiftBox (b1, b2) = (map shiftBox b1, map shiftBox b2)

box3vars :: [LinFunc (Three Box) Int]
box3vars = map var $ (\p q r -> Three (p, q, r)) <$> boxQs <*> boxQs <*> boxQs

shiftBox :: Box3 -> Box3
shiftBox (Three (a, b, c)) = Three (c, a, b)


-- choose :: [b] -> Int -> [[b]]
-- _      `choose` 0 = [[]]
-- []     `choose` _ =  []
-- (x:xs) `choose` k =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

summable :: Proposition Box3 -> Proposition Box3 -> Bool
summable p q = maxStateValue (p .@. q) <= 1

maxStateValue :: Proposition Box3 -> Double
maxStateValue p = unsafeSolveLP lp
  where
    lp = execLPM $ boxConstraints >> setDirection Max >> setObjective (toLinFunc p)

unsafeSolveLP :: LP Box3 Int -> Double
unsafeSolveLP f = case unsafePerformIO $ glpSolveVars glpkSolverOpts f of
  (Success, Just (v, _)) -> v
  _ -> error "Something bad happened"

box3varsMap :: Map.Map (Three Box) Int
box3varsMap = Map.fromList $ zipWith (curry swap) [0..] . map fst . concatMap Map.assocs $ box3vars

nonsignalingCMatrix :: [LinFunc Box3 Int]
nonsignalingCMatrix = map (uncurry (^-^)) nonsignaling

box3NoSignalingRank :: Int
box3NoSignalingRank = rank . coefficientMatrix box3varsMap $ nonsignalingCMatrix

equalP :: Proposition Box3 -> Proposition Box3 -> Bool
equalP p q = rank cmatrix == box3NoSignalingRank
  where
    pl = toLinFunc p
    ql = toLinFunc q
    cmatrix = coefficientMatrix box3varsMap $ (pl ^-^ ql):nonsignalingCMatrix

equalP' :: Proposition Box3 -> Proposition Box3 -> Bool
equalP' p q = equalZero (unsafeSolveLP maxlp) &&
              equalZero (unsafeSolveLP minlp)
  where
    equalZero x = abs x < 1e-6
    maxlp = execLPM $ boxConstraints >> setDirection Max >> setObjective lin
    minlp = execLPM $ boxConstraints >> setDirection Min >> setObjective lin
    lin = toLinFunc p ^-^ toLinFunc q

coefficientMatrix :: Map.Map Box3 Int -> [LinFunc Box3 Int] -> Matrix Double
coefficientMatrix vars = fromRows . map (coefficientVector vars)

-- |Convert LinFunc to hmatrix Vector.
-- vars is a monotonic Map from all vars appearing in LinFunc to Int
coefficientVector :: Map.Map Box3 Int -> LinFunc Box3 Int -> Vector Double
coefficientVector vars f = accum (konst 0 n) (+) keyvals
  where
    n = Map.size vars
    keyvals = map (varToInt *** fromIntegral) $ Map.assocs f
    varToInt k = fromMaybe (error "Key in LinFunc not in vars.")
                 $ Map.lookup k vars
-- |Parsers (adopted from QLogic.BoxWorld, we probably want to unify types)

instance (System a, Ord (a Box)) => Read (Proposition (a Box)) where
    readsPrec _ = either (const []) id . parseOnly parseQ' . pack
        where
            parseQ' = do
                q <- parseQ
                rest <- takeByteString
                return [(q, unpack rest)]

parseQ :: (System a, Ord (a Box)) => Parser (Proposition (a Box))
parseQ = do
    qs <- parseSAQ `sepBy` char '+'
    return . foldl1' (.@.) . map Atom $ qs

parseSAQ :: (System a) => Parser (a Box)
parseSAQ = do
    _ <- char '['
    q <- sequence $ pure parseAQ
    _ <- char ']'
    return q

parseAQ :: Parser Box
parseAQ =  do
  a <- letter_ascii
  alpha <- decimal
  return $ Box a alpha

makeAllSums :: [Proposition Box3] -> [Proposition Box3] -> [Proposition Box3]
makeAllSums e atoms = e ++ concatMap sums e
  where
    sums p = map (p .@.) $ filter (summable p) atoms

allSums :: [Proposition Box3] -> [Proposition Box3]
allSums []     = []
allSums (a:as) = a:sums ++ allSums as
  where
    sums =  map (a .@. ) . allSums .filter (summable a) $ as

-- unique [] = []
-- unique (a:as) = a:unique (filter (not . equalP a) as)

main :: IO ()
main = do
  --
  -- Localized elements that are not Mackey-compatible
  -- (Ref. Dvurecenskij, Pulmannova, New Trends... p. 63 for def)
  --
  let p1 = read "[X0X0Y0]"
      p2 = read "[X0X0Y1]"
      p = p1 .@. p2
      q1 = read "[X1Y1Y0]"
      q2 = read "[X1Y1Y1]"
      q  = q1 .@. q2
      rr = read "[X0X1Y0]+[X1Y0Y0]"
      pp = p2 .@. q2
      -- r1 = read "[X0X0Y0]"
      -- rr = read "[X0X1Y0]+[X1X0Y0]+[X1X1Y0]" :: Proposition Box3
      -- rr' = read "[X0Y1Y0]+[X1Y0Y0]+[X1Y1Y0]" :: Proposition Box3
      r1 = read "[X0X0Y0]+[X1Y1Y0]"
      r = r1 .@. rr
      zz = p1 .@. q1

  putStr "p and q summable: "
  print $ summable p q
  putStr "rr, p2, q2 summable: "
  print $ summable (p2 .@. q2) rr
  putStr "rr + zz equal to r: "
  print $ equalP (rr .@. zz) $ read "[X0X0Y0]+[X0X1Y0]+[X1X0Y0]+[X1X1Y0]"
  putStr "p + q == pp + zz: "
  print $ equalP (p .@. q) (pp .@. zz)
  putStr "pp rr zz summable: "
  print $ summable pp (rr .@. zz)

  putStr "Consistency: "
  print $ all (not . uncurry summable) $ zip box3Atoms box3Atoms

  -- let p1 = read "[X0X0Y0]+[X0X1Y0]"
  --     pp = read "[X0X0Y1]+[X0X1Y1]"
  --     p = p1 .@. pp
  --     q1 = read "[X0X0Y0]+[X0X1Y0]"
  --     qq = read "[X1X0Y0]+[X1X1Y0]"

  -- putStr "p1 and pp summable: "
  -- print $ summable p1 pp
  -- putStr "q1 and pp summable: "
  -- print $ summable q1 qq
  -- putStr "qq and pp summable: "
  -- print $ summable pp qq

  -- Various dev-tests
  ---
  -- print $ maxStateValue gyni
  -- let p = head box3Atoms
  --     sump p = filter (summable p) box3Atoms
  -- print $ length box3Atoms
  -- print $ length box3varsMap
  -- print box3NoSignalingRank
  -- let q1 = read "[X0X0X0]+[X0X0X1]" :: Proposition Box3
  --     q2 = read "[X0X0Y0]+[X0X0Y1]" :: Proposition Box3
  --     q3 = read "[X0Y0Y0]+[X0Y0Y1]" :: Proposition Box3
  --     x  = read "[X0X0X0]" :: Proposition Box3
  --     one = map read [ "[X0X0X0]", "[X0X0X1]", "[X0X1X0]", "[X0X1X1]"
  --                    , "[X1X0X0]", "[X1X0X1]", "[X1X1X0]", "[X1X1X1]"
  --                    ] :: [Proposition Box3]
  -- print $ equalP q1 q2
  -- print $ equalP q2 q3
  -- print $ summable x x
  -- -- let g1 = makeAllSums box3Atoms (take 8 box3Atoms)
  -- let g1 = makeAllSums box3Atoms box3Atoms
  --     g2 = makeAllSums g1 box3Atoms
  -- putStrLn $ unlines $ map show $ allSums (take 8 box3Atoms)
  -- putStrLn $ unlines $ map show $ allSums box3Atoms
  -- print $ length $ allSums (take 32 box3Atoms)
  -- print $ equalP <$> box3Atoms <*> box3Atoms
  -- print $ length g1
  -- print $ length g2
  -- print $ map (length . sump) box3Atoms
  -- print $ maxStateValue . foldl1' (.@.) $ one
  -- print $ map (summable (one !! 7)) $ scanl1 (.@.) one
  -- print $ map (`elem` box3Atoms) one
  -- print $ length $ allSums one
