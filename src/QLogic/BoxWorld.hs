{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module QLogic.BoxWorld where

import           Data.Maybe
import           Prelude                          hiding (and, concat,
                                                   foldl, foldl1, sequence)

import           Data.List (foldl1')
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet                      as IS
import           Data.Set                         (Set)
import qualified Data.Set                         as Set

import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map

-- import Data.Poset.ConcretePoset
import           QLogic
import           QLogic.Concrete
import           QLogic.States

import           Data.Attoparsec.ByteString.Char8 hiding (take)
import           Data.ByteString.Char8            (pack, unpack)

import           QLogic.Utils

import           Control.Applicative
import           Data.Foldable
import           Data.Traversable

import           Control.Monad                    (liftM)

-- |Represents discrete valued observable, named *name* with domain *domain*.
-- Domain is usually set [0..k], where k is number of distinct outputs.
-- Storing as a list is convenient for question construction.
data Observable = Observable { name   :: Char
                             , domain :: [Int] } deriving (Eq, Ord, Show)

-- |Point in the (generalized) phase space.
-- In classical physics, points of the phase space can be interpreted
-- as a pure states of the system. Consequently, each point specifies
-- exactly outcomes of observables of the classical system.
-- Here we use this interpretation encode points as a *Map* from *Char*
-- (symbol denoting observable) to *Int* (value of that observable).
newtype Point = Point (Map Char Int) deriving (Eq, Ord, Show)

-- |Phase space is simply a set of points.
newtype PhaseSpace a = PhaseSpace (Set a) deriving (Eq, Ord, Show)

-- |Get a list of points
phasePoints :: PhaseSpace a -> [a]
phasePoints (PhaseSpace a) = Set.toList a

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
class (Traversable a, Applicative a) => System a where

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

instance System One
instance System Two
instance System Three

-- |Applicative for single system is trivial,
-- and it seems that the only possible.
instance Applicative One where
    pure a = One a
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
-- > pure id <*> Two (v, w) = Two (id, id) <*> Two (v, w) = Two $ (id v, id w) = Two (v, w) -- OK
-- composition:
-- > pure (.) <*> Two (u, u') <*> Two (v, v') <*> Two (w, w')
-- > = Two ((.), (.)) <*> Two (u, u') <*> Two (v, v') <*> Two (w, w')
-- > = Two ( (.) u, (.) u' ) <*>  Two (v, v') <*> Two (w, w')
-- > = Two ( u . v, u . v' ) <*> Two (w, w') = Two (u . v $ w, u' . v' $ w')
-- on the other hand:
-- > Two (u, u') <*> (Two (v, v') <*> Two (w, w'))
-- > = Two (u, u') <*> Two (v w, v' w) = Two (u (v w), u' (v' w')) -- OK
-- homomorphism
-- > pure f <*> pure x = Two (f, f) <*> Two (x, x) = Two (f x, f x) = pure $ f x -- OK
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
combineWith :: (Traversable a, Applicative f) => (f b -> f c) -> a (f b) -> f (a c)
combineWith f = sequenceA . fmap f

-- |Build a phase space of a classical system, given the list of observables.
--
-- Let us discuss this function for Two system.
-- The argument is of type Two [Observable],
-- i.e. we have list of observables for each component of the system.
-- Then
-- > combineWith phaseSpace1' == sequenceA . fmap phaseSpace1'
-- firstly converts list of observables to list of
-- phaseSpace points that are required to describe output of these observables
-- (the 'fmap phaseSpace1'' part), and then we construct the list of
-- phase space points from the lists for components ('sequenceA').
phaseSpace :: (System a, Ord (a Point)) => a [Observable] -> PhaseSpace (a Point)
phaseSpace = PhaseSpace . Set.fromList . combineWith phaseSpace1'

phaseSpace1' :: [Observable] -> [Point]
phaseSpace1' obs = map (Point . Map.fromList) $ tups [[(name o, k) | k <- domain o] | o <- obs]
    where
        tups []     = []
        tups [v] = map (:[]) v
        tups (v:vs) = [p:ps | p <- v, ps <- tups vs]

data Atomic = Atomic Char Int | Null | Trivial deriving (Eq, Ord)
data Question a = Question a | OPlus (Question a) (Question a) deriving (Eq, Ord)

instance Show Atomic where
    show Trivial = "One"
    show Null    = "Null"
    show (Atomic a alpha) = a:show alpha

instance (Show a) => Show (Question a) where
   show (Question a) = show a
   show (OPlus a b) = show a ++ "+" ++ show b

infixl 4 <+>
(<+>) = OPlus

instance (Show a) => Show (One a) where
    show (One a) = "[" ++ show a ++ "]"

instance (Show a) => Show (Two a) where
    show (Two (a, b)) = "[" ++ show a ++ show b ++ "]"

instance (Show a) => Show (Three a) where
    show (Three (a, b, c)) = "[" ++ show a ++ show b ++ show c ++ "]"

askAtomicOne :: Atomic -> Point -> Bool
askAtomicOne (Atomic a alpha) (Point p) = case Map.lookup a p of
    Just x  -> x == alpha
    Nothing -> False
askAtomicOne Null _    = False
askAtomicOne Trivial _ = True

askQA :: (Foldable a, Applicative a) => Question (a Atomic) -> a Point -> Bool
askQA (Question a) p = and $ liftA2 askAtomicOne a p
askQA (OPlus a b) p  = askQA a p || askQA b p

atomicQuestionsOf :: Observable -> [Atomic]
atomicQuestionsOf obs = map (Atomic $ name obs) $ domain obs

phaseSubset :: (System a) => PhaseSpace (a Point) -> Question (a Atomic) -> Set (a Point)
phaseSubset (PhaseSpace points) q = Set.filter (askQA q) points

boxWorldAtomicQs :: (System a) => a [Observable] -> [Question (a Atomic)]
boxWorldAtomicQs obs = map Question . combineWith atomsOf $ obs
    where
        atomsOf = concat . map atomicQuestionsOf

boxWorldLogic :: (System c, Ord (c Point))
              => c [Observable]
              -> Representation ConcreteInt IntSet (Question (c Atomic))
boxWorldLogic obs = Representation q2set set2q ql
  where
    ql        = concreteIntSublogic (booleanAlgebraInt space) (map q2set atomicQs)
    phase     = phaseSpace obs
    atomicQs  = boxWorldAtomicQs obs
    phasePack = packList . phasePoints $ phase
    packSet   = IS.fromList . map (toKey phasePack) . Set.toList
    space     = IS.fromList [0..length atomicQs - 1]
    q2set     = packSet . phaseSubset phase
    invAtoms  = rightInvMap atomicQs q2set
    set2q q
        | IS.null q = Question . pure $ Null
        | otherwise = foldl1 (<+>) . map (\a -> fromJust $ Map.lookup a invAtoms) . decomp $ q
            where
                decomp = head . atomicDecomposition ql

rightInvMap :: (Ord b) => [a] -> (a -> b) -> Map b a
rightInvMap dom f = foldl' go Map.empty dom
    where
        go !accum x = Map.insert (f x) x accum

rightInv :: (Eq b) => [a] -> (a -> b) -> b -> a
rightInv dom f y = fromJust $ find (\x -> f x == y) dom

data Representation p a b  = Representation { toRepr    :: b -> a
                                            , fromRepr  :: a -> b
                                            , logicRepr :: p }

instance (POrdStruct p a) => POrdStruct (Representation p a b) b where
    elementsOf ql = map (fromRepr ql) . elementsOf . logicRepr $ ql
    lessIn        = liftRepr2 lessIn
    supIn ql a b  = liftM (fromRepr ql) $ liftRepr2 supIn ql a b
    infIn ql a b  = liftM (fromRepr ql) $ liftRepr2 infIn ql a b

instance (Eq b, QLogicStruct p a) => QLogicStruct (Representation p a b) b where
    ocmplIn  = liftRepr ocmplIn
    orthoIn  = liftRepr2 orthoIn
    compatIn = liftRepr2 compatIn
    zeroOf   = liftRepr0 zeroOf
    oneOf    = liftRepr0 oneOf
    -- subLogic iso els = Representation (toRepr iso) (fromRepr iso) sublogic
    --     where
    --         sublogic = subLogic (logicRepr iso) $ map (toRepr iso) els

-- I kinf of don't understand that.
--
-- stateRepr :: Representation p a b -> State b f -> State a f
-- stateRepr r (State s) = State $ \q -> s . fromRepr $ q 
-- 
-- But if we map atoms when we create the state,
-- then it is much more efficient (fromRepr is expensive)
--
-- There is some "hidden" structure, maybe it will give
-- some hints.
--

liftRepr2 f iso a b = f (logicRepr iso) (toRepr iso a) (toRepr iso b)
liftRepr f iso = fromRepr iso . f (logicRepr iso) . toRepr iso
liftRepr0 f iso = fromRepr iso $ f (logicRepr iso)

twoValuedStates :: ConcreteInt -> [State IntSet Int]
twoValuedStates ql = filter (isStateII' ql) . map (fromAtomicList ql . zip atoms) $ tuples [0, 1] n
    where
        atoms = atomsOf ql
        n = length atoms

-- Printers and readers

instance Read Atomic where
    readsPrec _ = either (const []) id . parseOnly parseAtomic . pack
        where
            parseAtomic = do
                p <- parseAQ
                rest <- takeByteString
                return [(p, unpack rest)]

parseAQ :: Parser Atomic
parseAQ = (string "Null" >> return Null) <|> 
          (string "One" >> return Trivial) <|> 
          parseAtomic
    where
        parseAtomic = do
            a <- letter_ascii
            alpha <- decimal
            return $ Atomic a alpha

parseSAQ :: (System a) => Parser (a Atomic)
parseSAQ = do
    char '['
    q <- sequence $ pure parseAQ
    char ']'
    return q

instance (System a) => Read (Question (a Atomic)) where
    readsPrec p = either (const []) id . parseOnly parseQ' . pack
        where
            parseQ' = do
                q <- parseQ
                rest <- takeByteString
                return [(q, unpack rest)]

parseQ :: (System a) => Parser (Question (a Atomic))
parseQ = do
    -- char '['
    qs <- parseSAQ `sepBy` (char '+') 
    -- char ']'
    return . foldl1' (<+>) . map Question $ qs

parsePRState :: System a => Parser [(Question (a Atomic), Double)]
parsePRState = parseValue `sepBy1` (char ',' >> skipSpace) 
    where
        parseValue = do 
            q <- parseQ
            char '='
            v <- double
            return (q, v)

-- readState2 :: String -> ConcreteInt -> (Question (Two Atomic) -> IntSet) -> State IntSet Double
-- readState2 str ql repr = case parseOnly (parseAtomicState repr) (pack str) of
--     Right qs -> fromAtomicList ql qs
--     Left err -> error $ "Error parsing" ++ err

-- parseAtomicState :: System a => (Question (a Atomic) -> IntSet) -> Parser [(IntSet, Double)]
-- parseAtomicState repr = parseValue' `sepBy1` (char ',' >> skipSpace)
--     where
--         parseValue' = do
--             q <- parseQ
--             char '='
--             v <- double
--             return (repr . Question $ q, v)
