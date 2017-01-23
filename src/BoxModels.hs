{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, GADTs #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
module BoxModels where

import Control.Arrow
import Data.List
import Data.Tuple
import Data.Maybe
import qualified Data.Map as Map

import Data.LinearProgram hiding (constraints, one)
import System.IO.Unsafe(unsafePerformIO)

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra

import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.ByteString.Char8 (pack, unpack)

-- |Represents discrete valued observable, named *name* with domain *domain*.
-- Domain is usually set [0..k], where k is number of distinct outputs.
-- Storing as a list is convenient for question construction.
data Observable = Observable { name   :: Char
                             , domain :: [Int] } deriving (Eq, Ord, Show)

-- |We define an alias for data type class that will represent
-- physical systems, single or composed.
--
-- We want to discuss either single or composite systems
-- in typesafe manner. We want to distinguish between
-- number of parties and have some tools to compose single
-- systems into composite systems. It appeared that
-- what we require is that a data type representing physical
-- system is *Traversable* and *Applicative*. Detailed
-- discussion of why can be find in instance declarations.
class (Traversable s, Applicative s) => System s where
  shifts   :: [s a -> s a]
  unshifts :: [s a -> s a]

-- |Types representing single system, and composite systems
-- consisting of two and three parties.
--
-- We have simple product structure, parametrized by one
-- type. The type does not distinguish different systems,
-- but type of the property that we discuss.
-- In this work, this is either the phase space point
-- or an elementary question (*Atomic*) about
-- some property.
--
-- The interesting thing is that only the *Applicative*
-- instance cannot be automaticaly derived.
--
-- We derive *Foldable* to be able to call *and* function.
-- For example, by asking a pair of questions p, q on
-- 2-party system we obtain as a result Two Bool type.
-- We want to reduce answer to composite system.
--
-- We derive *Functor*, because we want to transform
-- between considered properties. E.g. for the question
-- on two-party system we want to be able to compute
-- a phase space subset of two-party system.
--
-- *Traversable* instance is required because of
-- > sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- We use it only for lists. For example, having
-- Two [a] we would like to get [Two a].
-- Interpretation is straightforward: having a Two
-- lists of some qualites of components we want to construct
-- a list of qualities for the whole two-party system.
newtype One a = One a deriving (Eq, Foldable, Functor, Ord, Traversable)
newtype Two a = Two (a, a) deriving (Eq, Foldable, Functor, Ord, Traversable)
newtype Three a = Three (a, a, a) deriving (Eq, Foldable, Functor, Ord, Traversable)

one :: a -> One a
one = One

two :: a -> a -> Two a
two a b = Two (a, b)

three :: a -> a -> a -> Three a
three a b c = Three (a, b, c)

instance (Show a) => Show (One a) where
    show (One a) = "[" ++ show a ++ "]"

instance (Show a) => Show (Two a) where
    show (Two (a, b)) = "[" ++ show a ++ show b ++ "]"

instance (Show a) => Show (Three a) where
    show (Three (a, b, c)) = "[" ++ show a ++ show b ++ show c ++ "]"

instance System One where
  shifts   = [id]
  unshifts = [id]

instance System Two where
  shifts   = [id, \ (Two (a, b)) -> Two (b, a)]
  unshifts = [id, \ (Two (a, b)) -> Two (b, a)]

instance System Three where
  shifts = [ id, shift, shift . shift ]
    where
      shift (Three (a, b, c)) = Three (c, a, b)
  unshifts = [ id, shift, shift . shift ]
    where
      shift (Three (a, b, c)) = Three (b, c, a)

-- |Applicative for single system is trivial,
-- and it seems that the only possible.
instance Applicative One where
    pure = One
    (One f) <*> (One a) = One $ f a

-- Check laws:
-- identity:
-- > pure id <*> One v = One id <*> One v = One $ id v = One v -- OK
-- composition:
-- > pure (.) <*> One u <*> One v <*> One w = One (.) <*> One u <*> One v <*> One w
-- > = One ( (.) u ) <*> One v <*> One w = One ( u . v ) <*> One w = One (u . v $ w)
-- on the other hand:
-- > One u <*> (One v <*> One w) = One u <*> One (v w) = One $ u (v w) -- OK
-- homomorphism
-- > pure f <*> pure x = One f <*> One x = One $ f x = pure $ f x -- OK
-- interchange:
-- > One u <*> pure y = One $ u y
-- > pure ($ y) <*> One u = One ($ y) <*> One u = One (($ y) u) = One (u $ y) -- OK

-- |Applicative instance for two-party system is straightforward:
-- we have simple product structure.
instance Applicative Two where
    pure a = Two (a, a)
    (Two (f, g)) <*> (Two (a, b)) = Two (f a, g b)

-- Check laws:
-- identity:
-- > pure id <*> Two (v, w) = Two (id, id) <*> Two (v, w) =
-- >    Two $ (id v, id w) = Two (v, w) -- OK
-- composition:
-- > pure (.) <*> Two (u, u') <*> Two (v, v') <*> Two (w, w')
-- > = Two ((.), (.)) <*> Two (u, u') <*> Two (v, v') <*> Two (w, w')
-- > = Two ( (.) u, (.) u' ) <*>  Two (v, v') <*> Two (w, w')
-- > = Two ( u . v, u . v' ) <*> Two (w, w') = Two (u . v $ w, u' . v' $ w')
-- on the other hand:
-- > Two (u, u') <*> (Two (v, v') <*> Two (w, w'))
-- > = Two (u, u') <*> Two (v w, v' w) = Two (u (v w), u' (v' w')) -- OK
-- homomorphism
-- > pure f <*> pure x = Two (f, f) <*> Two (x, x)
-- >    = Two (f x, f x) = pure $ f x -- OK
-- interchange:
-- > Two (u, v) <*> pure y = Two (u y, v y)
-- > pure ($ y) <*> Two (u, v) = Two ($ y, $ y) <*> Two (u, v)
-- > = Two (($ y) u, ($ y) v) = Two (u $ y, v $ y) -- OK

-- |Analogously we define instance for three-party system.
instance Applicative Three where
    pure a = Three (a, a, a)
    (Three (f, g, h)) <*> (Three (a, b, c)) = Three (f a, g b, h c)

-- |Helper function. Like *sequenceA* for *One*, *Two*, etc. is used
-- only for lists here.
-- This function specialize to
-- > combineWith :: ([b] -> [c]) -> a [b] -> a [c].
-- This simply applies some function that transforms list of properties
-- and then we construct list for composite system from
-- lists for components.
-- TODO improve this documentation.
combineWith :: (Traversable a, Applicative f) => (b -> f c) -> a b -> f (a c)
combineWith f = sequenceA . fmap f

data Box = Box !Char !Int deriving (Eq, Ord)

instance Show Box where
  show (Box o i) = o : show i

newtype Question a = Question [a] deriving (Eq, Ord, Functor, Foldable)

atomicQ :: a -> Question a
atomicQ = Question . (:[])

nullQ :: Question a
nullQ = Question []

instance Show a => Show (Question a) where
  show (Question []) = "NULL"
  show (Question a) = intercalate "+" . map show $ a

-- TODO check laws
instance Applicative Question where
  pure = Question . (:[])
  (Question a) <*> (Question b) = Question $ a <*> b

(.@.) :: Ord a => Question a -> Question a -> Question a
(Question a) .@. (Question b) = Question . sort $ a ++ b

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
-- boxAtoms = map Atom . boxQs
boxAtoms = map pure . boxQs

binaryO :: Char -> Observable
binaryO x = Observable x [0, 1]

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

nonsignaling' :: (Ord (s Box), Splitting s s') =>
                 BoxModel s -> [(Question (s Box), Question (s Box))]
nonsignaling' model = combineNS <$> pairs a <*> rest
  where
    (a, rest) = map Question . idDecompositions *** boxAtoms $ split model
    combineNS (l, r) q = (combineQ l q, combineQ r q)
    combineQ p q = curry combine <$> p <*> q

nonsignaling :: (Ord (s Box), Splitting s s') =>
                BoxModel s -> [(Question (s Box), Question (s Box))]
nonsignaling model = concatMap ns $ zip unshifts shifts
  where
    ns (post, pre) = fmap (fmap post *** fmap post) . nonsignaling' $ pre model

--
-- * General state computations
--
-- A state on a box model is defined by its values on atomic
-- propositions. For an arbitrary state, these are the unknown
-- values.

--
-- | Applies a general state to a question. Result is a linear function.
toLinearFunc :: (Ord (s Box), System s) => Question (s Box) -> LinFunc (s Box) Int
toLinearFunc (Question a) = varSum a

-- | General state acting on decompositions of identity
normalizationC :: (Ord (s Box), System s) => BoxModel s -> [LinFunc (s Box) Int]
normalizationC = map varSum . idDecompositions

-- | No-signaling conditions as pairs of linear functions.
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

coefficientMatrix :: Ord a => Map.Map a Int -> [LinFunc a Int] -> Matrix Double
coefficientMatrix vars = fromRows . map (coefficientVector vars)

-- |Convert LinFunc to hmatrix Vector.
-- vars is a monotonic Map from all vars appearing in LinFunc to Int
coefficientVector :: Ord a => Map.Map a Int -> LinFunc a Int -> Vector Double
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

oneBinaryBox = let x = binaryO 'X'
                   y = binaryO 'Y' in
                 one [x, y]

twoBinaryBoxes = let x = binaryO 'X'
                     y = binaryO 'Y' in
                   two [x, y] [x, y]

threeBinryBoxes = let x = binaryO 'X'
                      y = binaryO 'Y' in
                    three [x, y] [x, y] [x, y]

--
-- Readers
--

instance (System s, Ord (s Box)) => Read (Question (s Box)) where
    readsPrec _ = either (const []) id . parseOnly parseQ' . pack
        where
            parseQ' = do
                q <- parseQ
                rest <- takeByteString
                return [(q, unpack rest)]

parseQ :: (System s, Ord (s Box)) => Parser (Question (s Box))
parseQ = do
    qs <- parseSAQ `sepBy` char '+'
    return . Question . sort  $ qs

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

--
-- Helper
--

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a:as) = map ((,) a) as ++ pairs as
