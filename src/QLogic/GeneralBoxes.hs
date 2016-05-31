{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
module QLogic.GeneralBoxes where

import Control.Arrow
import Data.List
import Data.Tuple
import Data.Maybe
import qualified Data.Map as Map

import Data.LinearProgram hiding (constraints)
import System.IO.Unsafe(unsafePerformIO)

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra

import QLogic.Utils
import QLogic.BoxWorld hiding (Question)

data Box = Box !Char !Int deriving (Eq, Ord)

instance Show Box where
  show (Box o i) = o : show i

infixr 4 :@:

data Question a = Atom !a | !a :@: !(Question a)
  deriving (Ord, Eq, Functor, Foldable, Traversable)

instance Show a => Show (Question a) where
  show (Atom a) = show a
  show (a :@: b) = show a ++ "+" ++ show b

(.@.) :: Ord a => Question a -> Question a -> Question a
(Atom a) .@. (Atom b)        | a <= b    = a :@: Atom  b
                             | otherwise = b :@: Atom a
(Atom a) .@. p@(b :@: c)     | a <= b    = a :@: p
                             | otherwise = b :@: (Atom a .@. c)
(a :@: b) .@. p = Atom a .@. b .@. p

infixr 5 .@.

type BoxModel s = s [Observable]

-- |All box questions for a given observable (i.e. input)
boxes' :: Observable -> [Box]
boxes' Observable{name = n, domain = d} = map (Box n) d

boxes :: System s => s Observable -> [s Box]
boxes = combineWith boxes'

boxQs :: System s => BoxModel s -> [s Box]
boxQs = concatMap boxes . sequenceA

-- |All atomic propositions on a three box system
boxAtoms :: System s => BoxModel s -> [Question (s Box)]
boxAtoms = map Atom . boxQs

x :: Observable
x = Observable 'X' [0, 1]

y :: Observable
y = Observable 'Y' [0, 1]

-- |Identity decompositions in a box model
-- Here is how it works:
--    model :: System s => s [Observable]
-- describes the box model. Each part of a system is described
-- by a list of observables.
--
-- The:
--   fmap (map boxes') model
--
-- calls (map boxes') on each component of a model.
-- The latter action results in a list of questions
-- about a particular observation. Sum of these
-- questions gives identity.
--
-- Finally the last sequenceA's perform product action
-- on the
--   fmap (map boxes') model :: s [[Box]]
-- resulting in a type [[s Box]]
-- in the only possible way.
idDecompositions :: System s => BoxModel s -> [[s Box]]
idDecompositions = map sequenceA . combineWith (map boxes')

class (System s, System s') => Splitting s s' | s -> s' where
  split :: s a -> (One a, s' a)
  combine :: (One a, s' a) -> s a

instance Splitting Two One where
  split (Two (a, b)) = (One a, One b)
  combine (One a, One b) = Two (a, b)

instance Splitting Three Two where
  split (Three (a, b, c)) = (One a, Two (b, c))
  combine (One a, Two (b, c)) = Three (a, b, c)

combineQ :: (Ord (s Box), Splitting s s') =>
            Question (One Box) -> Question (s' Box) -> Question (s Box)
combineQ (Atom a) (Atom b) = Atom $ combine (a, b)
combineQ a@(Atom _) q = foldl1 (.@.) $ fmap (combineQ a . Atom) q
combineQ (a :@: as) q = combineQ (Atom a) q .@. combineQ as q

nonsignaling' :: (Ord (s Box), Splitting s s') =>
                 BoxModel s -> [(Question (s Box), Question (s Box))]
nonsignaling' model = combineNS <$> pairs a <*> rest
  where
    (a, rest) = map sumUp . idDecompositions *** boxAtoms $ split model
    sumUp = foldl1' (.@.) . map Atom
    combineNS (l, r) q = (combineQ l q, combineQ r q)

nonsignaling :: (Ord (s Box), Splitting s s') =>
                BoxModel s -> [(Question (s Box), Question (s Box))]
nonsignaling model = concatMap ns $ zip unshifts shifts
  where
    ns (post, pre) = fmap (fmap post *** fmap post) . nonsignaling' $ pre model

--
-- Linear programming tools
--
toLinearFunc :: (Ord (s Box), System s) => Question (s Box) -> LinFunc (s Box) Int
toLinearFunc (Atom a) = var a
toLinearFunc (a :@: b) = var a ^+^ toLinearFunc b

normalizationC :: (Ord (s Box), System s) => BoxModel s -> [LinFunc (s Box) Int]
normalizationC = map varSum . idDecompositions

nonsignalingC :: (Ord (s Box), Splitting s s') =>
                 BoxModel s -> [(LinFunc (s Box) Int, LinFunc (s Box) Int)]
nonsignalingC = map (toLinearFunc *** toLinearFunc) . nonsignaling

data BMC s = BMC { constraints :: LPM (s Box) Int ()
                 , nsrankValue :: Int
                 , nsLinFunc   :: [LinFunc (s Box) Int]
                 , boxVarsMap  :: Map.Map (s Box) Int}

boxConstraints :: (Ord (s Box), Splitting s s') => BoxModel s -> BMC s
boxConstraints model = BMC lpc nsrank nsCM bvm
  where
    boxvars = map var $ boxQs model
    nsc = nonsignalingC model
    lpc = do
      mapM_ (uncurry equal) nsc
      mapM_ (`equalTo` 1) $ normalizationC model
      mapM_ (`constrain` Bound 0 1) boxvars
    nsCM = map (uncurry (^-^)) nsc
    bvm = Map.fromList . zipWith (curry swap) [0..] . map fst .
          concatMap Map.assocs $ boxvars
    nsrank = rank . coefficientMatrix bvm $ nsCM

coefficientMatrix :: Ord (s Box) =>
  Map.Map (s Box) Int -> [LinFunc (s Box) Int] -> Matrix Double
coefficientMatrix vars = fromRows . map (coefficientVector vars)

-- |Convert LinFunc to hmatrix Vector.
-- vars is a monotonic Map from all vars appearing in LinFunc to Int
coefficientVector :: Ord (s Box) => Map.Map (s Box) Int -> LinFunc (s Box) Int -> Vector Double
coefficientVector vars f = accum (konst 0 n) (+) keyvals
  where
    n = Map.size vars
    keyvals = map (varToInt *** fromIntegral) $ Map.assocs f
    varToInt k = fromMaybe (error "Key in LinFunc not in vars.")
                 $ Map.lookup k vars

unsafeSolveLP :: Ord (s Box) => LP (s Box) Int -> Double
unsafeSolveLP f = case unsafePerformIO $ glpSolveVars glpkSolverOpts f of
  (Success, Just (v, _)) -> v
  _ -> error "Something bad happened"

glpkSolverOpts :: GLPOpts
glpkSolverOpts = MipOpts MsgOff 10000 True DrTom LocBound AllPre False [] 0.0

summable :: (Ord (s Box), System s) =>
            BMC s -> Question (s Box) -> Question (s Box) -> Bool
summable c p q = maxStateValue c (p .@. q) <= 1

maxStateValue :: (Ord (s Box), System s) =>
                 BMC s -> Question (s Box) -> Double
maxStateValue c p = unsafeSolveLP lp
  where
    lp = execLPM $ constraints c >> setDirection Max >> setObjective (toLinearFunc p)

equalP :: (Ord (s Box), System s) =>
  BMC s -> Question (s Box) -> Question (s Box) -> Bool
equalP BMC{boxVarsMap = bvm, nsLinFunc = nsc, nsrankValue = nsrank} p q =
  rank cmatrix == nsrank
  where
    pl = toLinearFunc p
    ql = toLinearFunc q
    cmatrix = coefficientMatrix bvm $ (pl ^-^ ql):nsc

allBoxModelUniqueQuestions :: (Ord (s Box), Splitting s s') =>
  BoxModel s -> [Question (s Box)]
allBoxModelUniqueQuestions model =
  nubBy (equalP c) . allBoxModelQuestions' c . boxAtoms $ model
  where
    c = boxConstraints model

allBoxModelQuestions :: (Ord (s Box), Splitting s s') =>
  BoxModel s -> [Question (s Box)]
allBoxModelQuestions model = allBoxModelQuestions' c . boxAtoms $ model
  where
    c = boxConstraints model

allBoxModelQuestions' _ []     = []
allBoxModelQuestions' c (a:as) = a:sums ++ allBoxModelQuestions' c as
  where
    sums = map (a .@.) . allBoxModelQuestions' c . filter (summable c a) $ as



-- boxModelPropositions' _ _ [] = []
-- boxModelPropositions' c ps pool = undefined
--   where
--     new = mergeEquivClasses $ map makeSums ps
--     makeSums p = map (p `liftedPlus`) . filter (liftFunc2 (summable c) p) $ pool
--     (Equiv as) `liftedPlus` (Equiv bs) = Equiv $ (.@.) <$> as <*> bs


-- mergeEquivClasses :: (Equiv a -> Equiv a -> Bool) -> [Equiv a] -> [Equiv a]
-- mergeEquivClasses eq (a:as) = merged ++ mergeEquivClasses rest
--   where
--     merged = Equiv $ concatMap (\(Equiv x) -> x) . filter (eq a) as
--     rest = filter (not . eq a) as

-- -- |Class for quotient data types
-- class Quotient a b where
--   quotient :: a -> b
--   repr     :: b -> a

-- -- |Lifts a function to a quotient
-- -- Function must respect equivalence classes
-- liftQ :: Quotient a b => (a -> a) -> b -> b
-- liftQ f = quotient . f . repr

-- -- |Lifts a binary function to a quotient
-- -- Function must respect equivalence classes
-- liftQ2 :: Quotient a b => (a -> a -> c) -> b -> b -> c
-- liftQ2 f a b = f (repr a) (repr b)

-- type Equiv a = [a]

--
-- Models
--

twoboxes = two [x, y] [x, y]

threeboxes = three [x, y] [x, y] [x, y]

--
-- Helper
--

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a:as) = map ((,) a) as ++ pairs as
