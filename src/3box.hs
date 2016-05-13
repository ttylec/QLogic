{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Main (main) where

import Control.Arrow
import Control.Monad
import Control.Applicative hiding (Const)
import Data.List
import Data.Tuple
import Data.Foldable
import Data.Monoid
import qualified Data.Map as Map

import QLogic.BoxWorld (System, One, Two, Three(Three))

-- Linear programming
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.LPMonad
import Data.LinearProgram
import Data.LinearProgram.GLPK
import Data.Algebra

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
idDecomp1 = [boxes 'X', boxes 'Y']
-- |in three boxes (thanks to Traversable Three!)
idDecomp3 = three <$> idDecomp1 <*> idDecomp1 <*> idDecomp1

gyni = foldl1' (.@.) $ map Atom gyniboxes

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

gynimax :: LP Box3 Int
gynimax = execLPM $
  do
    boxConstraints
    setDirection Max
    setObjective (varSum gyniboxes)

glpkSolverOpts = MipOpts MsgOff 10000 True DrTom LocBound AllPre False [] 0.0

-- |Helper function since Three requires a triple
three a b c = Three (a, b, c)

normalization :: [LinFunc Box3 Int]
normalization = map (varSum . sequenceA) idDecomp3

nonsignaling' :: Box -> Box -> ([Box3], [Box3])
nonsignaling' p q = (lhs, rhs)
  where
    lhs = map (three p q) $ boxes 'X'
    rhs = map (three p q) $ boxes 'Y'

nonsignaling :: [(LinFunc Box3 Int, LinFunc Box3 Int)]
nonsignaling = map (varSum *** varSum) $ concat [one, two, three]
  where
    one = map (uncurry nonsignaling') pqs
    two = map (mapShiftBox . uncurry nonsignaling') pqs
    three = map (mapShiftBox . mapShiftBox . uncurry nonsignaling') pqs
    pqs = (,) <$> boxQs <*> boxQs
    mapShiftBox (b1, b2) = (map shiftBox b1, map shiftBox b2)

box3vars = map var $ (\p q r -> Three (p, q, r)) <$> boxQs <*> boxQs <*> boxQs

shiftBox :: Box3 -> Box3
shiftBox (Three (a, b, c)) = Three (c, a, b)


choose :: [b] -> Int -> [[b]]
_      `choose` 0 = [[]]
[]     `choose` _ =  []
(x:xs) `choose` k =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

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

box3varsMap = Map.fromList $ map swap . zip [0..] . map fst . concatMap Map.assocs $ box3vars
nonsignalingCMatrix = map (\(lhs, rhs) -> lhs ^-^ rhs) nonsignaling
box3NoSignalingRank = rank . coefficientMatrix box3varsMap $ nonsignalingCMatrix

equalP :: Proposition Box3 -> Proposition Box3 -> Bool
equalP p q = rank cmatrix == box3NoSignalingRank
  where
    pl = toLinFunc p
    ql = toLinFunc q
    cmatrix = coefficientMatrix box3varsMap $ (pl ^-^ ql):nonsignalingCMatrix

equalP' :: Proposition Box3 -> Proposition Box3 -> Bool
equalP' p q = (equalZero $ unsafeSolveLP maxlp) &&
              (equalZero $ unsafeSolveLP minlp)
  where
    equalZero x = abs x < 1e-6
    maxlp = execLPM $ boxConstraints >> setDirection Max >> setObjective lin
    minlp = execLPM $ boxConstraints >> setDirection Min >> setObjective lin
    lin = (toLinFunc p) ^-^ (toLinFunc q)

coefficientMatrix :: Map.Map Box3 Int -> [LinFunc Box3 Int] -> Matrix Double
coefficientMatrix vars = fromRows . map (coefficientVector vars)

-- |Convert LinFunc to hmatrix Vector.
-- vars is a monotonic Map from all vars appearing in LinFunc to Int
coefficientVector :: Map.Map Box3 Int -> LinFunc Box3 Int -> Vector Double
coefficientVector vars f = accum (konst 0 n) (+) $ map (\(k, v) -> (varToInt k, fromIntegral v)) $ Map.assocs f
  where
    n = Map.size vars
    varToInt k = case Map.lookup k vars of
      Just k -> k
      Nothing -> error "Key in LinFunc not in vars."

-- |Parsers (adopted from QLogic.BoxWorld, we probably want to unify types)

instance (System a, Ord (a Box)) => Read (Proposition (a Box)) where
    readsPrec p = either (const []) id . parseOnly parseQ' . pack
        where
            parseQ' = do
                q <- parseQ
                rest <- takeByteString
                return [(q, unpack rest)]

parseQ :: (System a, Ord (a Box)) => Parser (Proposition (a Box))
parseQ = do
    qs <- parseSAQ `sepBy` (char '+')
    return . foldl1' (.@.) . map Atom $ qs

parseSAQ :: (System a) => Parser (a Box)
parseSAQ = do
    char '['
    q <- sequence $ pure parseAQ
    char ']'
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

unique [] = []
unique (a:as) = a:unique (filter (not . equalP a) as)

main :: IO ()
main = do
  print $ maxStateValue gyni
  let p = head box3Atoms
      sump p = filter (summable p) box3Atoms
  print $ length box3Atoms
  print $ length box3varsMap
  print $ box3NoSignalingRank
  let q1 = read "[X0X0X0]+[X0X0X1]" :: Proposition Box3
      q2 = read "[X0X0Y0]+[X0X0Y1]" :: Proposition Box3
      q3 = read "[X0Y0Y0]+[X0Y0Y1]" :: Proposition Box3
      x  = read "[X0X0X0]" :: Proposition Box3
      one = map read [ "[X0X0X0]", "[X0X0X1]", "[X0X1X0]", "[X0X1X1]"
                     , "[X1X0X0]", "[X1X0X1]", "[X1X1X0]", "[X1X1X1]"
                     ] :: [Proposition Box3]
  print $ equalP q1 q2
  print $ equalP q2 q3
  print $ summable x x
  -- let g1 = makeAllSums box3Atoms (take 8 box3Atoms)
  let g1 = makeAllSums box3Atoms box3Atoms
      g2 = makeAllSums g1 box3Atoms
  -- putStrLn $ unlines $ map show $ allSums (take 8 box3Atoms)
  -- putStrLn $ unlines $ map show $ allSums box3Atoms
  print $ length $ allSums (take 32 box3Atoms)
  -- print $ equalP <$> box3Atoms <*> box3Atoms
  -- print $ length g1
  -- print $ length g2
  -- print $ map (length . sump) box3Atoms
  -- print $ maxStateValue . foldl1' (.@.) $ one
  -- print $ map (summable (one !! 7)) $ scanl1 (.@.) one
  -- print $ map (`elem` box3Atoms) one
  -- print $ length $ allSums one
