{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module BoxModels.SetRepresentation where

-- TODO add export list

import Data.Maybe
import Data.List
import Prelude hiding (and, concat
                      , foldl, foldl1, sequence)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Attoparsec.ByteString.Char8 hiding (take, space)

import Control.Applicative

import BoxModels

-- |Point in the (generalized) phase space.
-- In classical physics, points of the phase space can be interpreted
-- as a pure states of the system. Consequently, each point specifies
-- exactly outcomes of observables of the classical system.
-- Here we use this interpretation encode points as a *Map* from *Char*
-- (symbol denoting observable) to *Int* (value of that observable).
newtype Point = Point (Map Char Int) deriving (Eq, Ord)

instance Show Point where
  show (Point p) = concatMap (\(k, v) -> show k ++ show v) . Map.toList $ p

-- |Phase space is simply a set of points.
newtype PhaseSpace a = PhaseSpace (Set a) deriving (Eq, Ord, Show)

-- |Get a list of points
phasePoints :: PhaseSpace a -> [a]
phasePoints (PhaseSpace a) = Set.toList a

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

askBox :: System s => s Box -> s Point -> Bool
askBox b point = and $ liftA2 askBox1 b point
  where
    askBox1 (Box a alpha) (Point p) = maybe False (== alpha) $ Map.lookup a p

askQ :: System s => Question (s Box) -> s Point -> Bool
askQ q point = or $ fmap (`askBox` point) q

phaseSubset :: System s => PhaseSpace (s Point) -> Question (s Box) -> Set (s Point)
phaseSubset (PhaseSpace points) q = Set.filter (askQ q) points


-- rightInvMap :: (Ord b) => [a] -> (a -> b) -> Map b a
-- rightInvMap dom f = foldl' go Map.empty dom
--     where
--         go !accum x = Map.insert (f x) x accum

-- rightInv :: (Eq b) => [a] -> (a -> b) -> b -> a
-- rightInv dom f y = fromJust $ find (\x -> f x == y) dom

-- data Representation p a b  = Representation { toRepr    :: b -> a
--                                             , fromRepr  :: a -> b
--                                             , logicRepr :: p }

-- instance (POrdStruct p a) => POrdStruct (Representation p a b) b where
--     elementsOf ql = map (fromRepr ql) . elementsOf . logicRepr $ ql
--     lessIn        = liftRepr2 lessIn
--     supIn ql a b  = fromRepr ql <$> liftRepr2 supIn ql a b
--     infIn ql a b  = fromRepr ql <$> liftRepr2 infIn ql a b

-- instance (Eq b, QLogicStruct p a) => QLogicStruct (Representation p a b) b where
--     ocmplIn  = liftRepr ocmplIn
--     orthoIn  = liftRepr2 orthoIn
--     compatIn = liftRepr2 compatIn
--     zeroOf   = liftRepr0 zeroOf
--     oneOf    = liftRepr0 oneOf
--     -- subLogic iso els = Representation (toRepr iso) (fromRepr iso) sublogic
--     --     where
--     --         sublogic = subLogic (logicRepr iso) $ map (toRepr iso) els


-- liftRepr2 f iso a b = f (logicRepr iso) (toRepr iso a) (toRepr iso b)
-- liftRepr f iso = fromRepr iso . f (logicRepr iso) . toRepr iso
-- liftRepr0 f iso = fromRepr iso $ f (logicRepr iso)

-- twoValuedStates :: ConcreteInt -> [State IntSet Int]
-- twoValuedStates ql = filter (isStateII' ql) . map (fromAtomicList ql . zip atoms) $ tuples [0, 1] n
--     where
--         atoms = atomsOf ql
--         n = length atoms

-- -- Printers and readers

-- parsePRState :: (System s, Ord (s Box)) => Parser [(Question (s Box), Double)]
-- parsePRState = parseValue `sepBy1` (char ',' >> skipSpace)
--     where
--         parseValue = do
--             q <- parseQ
--             char '='
--             v <- double
--             return (q, v)

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