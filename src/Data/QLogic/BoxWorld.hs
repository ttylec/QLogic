{-# LANGUAGE GADTs, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts #-} 
module Data.QLogic.BoxWorld where

import Prelude hiding (concat, foldl1, find, foldl, and, sequence)
import Data.List (intercalate, permutations)
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Poset.ConcretePoset
import Data.QLogic
import Data.QLogic.States

import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.ByteString.Char8 (pack, unpack)

import Data.QLogic.Utils

import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.Monoid

import Debug.Trace

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
newtype PhaseSpace a = PhaseSpace (Set a) deriving (Show)

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
-- or an elementary question (*AtomicQuestion*) about
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
-- only for lists here, this function also specialize to
-- > combineWith :: ([b] -> [c]) -> a [b] -> a [c].
-- This simply applies some function that transforms list of properties
-- and then we construct list for composite system from
-- lists for components.
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
phaseSpace1' obs = map (Point . Map.fromList) $ tuples [[(name o, k) | k <- domain o] | o <- obs]
    where
        tuples (v:[]) = map (:[]) v
        tuples (v:vs) = [p:ps | p <- v, ps <- tuples vs]

data AtomicQuestion = AtomicQuestion Char Int | Null | Trivial
data Question a = Primitive a | OPlus (Question a) (Question a)

instance Show AtomicQuestion where
    show (AtomicQuestion a alpha) = a:show alpha

instance (Show a) => Show (Question a) where
   show (Primitive a) = show a
   show (OPlus a b) = show a ++ "+" ++ show b

infixl 4 <+>
(<+>) = OPlus

instance (Show a) => Show (One a) where
    show (One a) = "[" ++ show a ++ "]"

instance (Show a) => Show (Two a) where
    show (Two (a, b)) = "[" ++ show a ++ show b ++ "]"

instance (Show a) => Show (Three a) where
    show (Three (a, b, c)) = "[" ++ show a ++ show b ++ show c ++ "]"

parseQ :: (System a) => Parser (a AtomicQuestion)
parseQ = do
    char '['
    qs <- sequence $ pure parseQ1  
    char ']'
    return qs

parseQ1 :: Parser AtomicQuestion
parseQ1 = do
    a <- letter_ascii 
    alpha <- decimal
    return $ AtomicQuestion a alpha
    
askAtomicOne :: AtomicQuestion -> Point -> Bool
askAtomicOne (AtomicQuestion a alpha) (Point p) = case Map.lookup a p of
        Just x  -> x == alpha
        Nothing -> False
askAtomicOne Null _          = False
askAtomicOne Trivial _       = True

askQA :: (Foldable a, Applicative a) => Question (a AtomicQuestion) -> a Point -> Bool
askQA (Primitive a) p = and $ liftA2 askAtomicOne a p 
askQA (OPlus a b) p   = askQA a p || askQA b p 

atomicQuestionsOf :: Observable -> [AtomicQuestion]
atomicQuestionsOf obs = map (AtomicQuestion $ name obs) $ domain obs

phaseSubset :: (System a) => PhaseSpace (a Point) -> Question (a AtomicQuestion) -> Set (a Point)
phaseSubset (PhaseSpace points) q = Set.filter (askQA q) points

boxWorldAtomicQs :: (System a) => a [Observable] -> [Question (a AtomicQuestion)]
boxWorldAtomicQs obs = map Primitive . combineWith atomsOf $ obs
    where
        atomsOf = concat . map atomicQuestionsOf 

boxWorldLogic :: (System c, Ord (c Point)) 
              => c [Observable] 
              -> (Question (c AtomicQuestion) -> IntSet, IntSet -> [Question (c AtomicQuestion)], CLogic)
boxWorldLogic obs = (q2set, set2q, ql)
    where
        ql        = CLogic $ QLogic poset (IS.difference space) IS.empty space
        poset     = boxWorldLogicPoset q2set atomicQs
        phase     = phaseSpace obs
        atomicQs  = boxWorldAtomicQs obs
        phasePack = packList . phasePoints $ phase
        packSet   = IS.fromList . map (toKey phasePack) . Set.toList
        unpackSet = Set.fromList . map (fromKey phasePack) . IS.toList
        space     = IS.fromList [0..length atomicQs - 1]
        q2set     = packSet . phaseSubset phase
        set2q     = map (foldl1 (<+>) . map atomicSet2q) . atomicDecomposition ql  
        atomicSet2q s = fromJust $ find ((s==) . q2set) atomicQs

boxWorldLogicPoset q2set = ConcretePosetInt . ordNub . generateConcreteElems . map q2set

ordNub :: (Ord a) => [a] -> [a]
ordNub = Set.toList . Set.fromList

generateConcreteElems :: [IntSet] -> [IntSet]
generateConcreteElems [] = [IS.empty]
generateConcreteElems (a:as) = generateConcreteElems as ++ 
                                    map (IS.union a) (generateConcreteElems disjoint)
    where
        disjoint = filter (IS.null . IS.intersection a) as

twoValuedStates :: CLogic -> [State IntSet Int]
twoValuedStates ql = filter (isStateII' ql) . map (fromAtomicList ql . zip atoms) $ tuples [0, 1] n
    where
        atoms = atomsOf ql
        n = length atoms

-- Printers and readers

readQ2 :: String -> Question (Two AtomicQuestion)
readQ2 str = case parseOnly parseQ2 $ pack str of 
    Right q  -> q
    Left err -> error $ "Error parsing: " ++ err

parseQ2 :: Parser (Question (Two AtomicQuestion))
parseQ2 = do
    (q:qs) <- parseQ2Atom `sepBy` (char '+')
    return $ foldl' (<+>) q qs

parseQ2Atom :: Parser (Question (Two AtomicQuestion))
parseQ2Atom = do
    char '['
    a <- letter_ascii
    alpha <- decimal
    b <- letter_ascii
    beta <- decimal
    char ']'
    return . Primitive $ Two (AtomicQuestion a alpha, AtomicQuestion b beta) 

readQ3 :: String -> Question (Three AtomicQuestion)
readQ3 str = case parseOnly parseQ3 $ pack str of 
    Right q  -> q
    Left err -> error $ "Error parsing: " ++ err

parseQ3 :: Parser (Question (Three AtomicQuestion))
parseQ3 = do
    (q:qs) <- parseQ3Atom `sepBy` (char '+')
    return $ foldl' (<+>) q qs

parseQ3Atom :: Parser (Question (Three AtomicQuestion))
parseQ3Atom = do
    char '['
    a <- letter_ascii
    alpha <- decimal
    b <- letter_ascii
    beta <- decimal
    c <- letter_ascii
    gamma <- decimal
    char ']'
    return . Primitive $ Three (AtomicQuestion a alpha, AtomicQuestion b beta, AtomicQuestion c gamma) 


readState2 :: String -> CLogic -> (Question (Two AtomicQuestion) -> IntSet) -> State IntSet Double
readState2 str ql repr = case parseOnly (parseAtomicState repr) (pack str) of
    Right qs -> fromAtomicList ql qs
    Left err -> error $ "Error parsing" ++ err

parseAtomicState :: System a => (Question (a AtomicQuestion) -> IntSet) -> Parser [(IntSet, Double)]
parseAtomicState repr = parseValue' `sepBy1` (char ',' >> skipSpace)
    where
        parseValue' = do
            q <- parseQ
            char '='
            v <- double
            return (repr . Primitive $ q, v)

-- Convert this to pretty printer
-- instance Show Point where
--     show (Point p) = "(" ++ (intercalate "," . map fmt . Map.toList $ p) ++ ")"
--         where
--             fmt (k, x) = k:'=':show x
