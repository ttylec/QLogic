{-# LANGUAGE GADTs, DeriveFunctor, DeriveFoldable #-} 
module Data.QLogic.BoxWorld where

import Prelude hiding (concat, foldl1, find, foldl, and)
import Data.List (intercalate)
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Poset.ConcretePoset
import Data.QLogic

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack, unpack)

import Data.QLogic.Utils

import Control.Applicative
import Data.Foldable
import Data.Monoid

data Observable = Observable { name   :: Char
                             , domain :: [Int] } deriving (Eq, Ord, Show)

newtype OneSystem = OneSystem (Map Char Int) deriving (Eq, Ord)

instance Show OneSystem where
    show (OneSystem p) = "(" ++ (intercalate "," . map fmt . Map.toList $ p) ++ ")"
        where
            fmt (k, x) = k:'=':show x

newtype PhaseSpace a = PhaseSpace (Set a) deriving (Show)

phasePoints :: PhaseSpace a -> [a]
phasePoints (PhaseSpace a) = Set.toList a

phasePointsSet :: PhaseSpace a -> Set a
phasePointsSet (PhaseSpace a) = a

phaseSpace :: (Composite a, Ord (a OneSystem)) => a [Observable] -> PhaseSpace (a OneSystem)
phaseSpace obs = PhaseSpace $ Set.fromList . compose . fmap phaseSpace1' $ obs

phaseSpace1' :: [Observable] -> [OneSystem]
phaseSpace1' obs = map (OneSystem . Map.fromList) $ tuples [[(name o, k) | k <- domain o] | o <- obs]
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

newtype One a = One a deriving (Eq, Foldable, Functor, Ord)
newtype Two a = Two (a, a) deriving (Eq, Foldable, Functor, Ord)
newtype Three a = Three (a, a, a) deriving (Eq, Ord)  --Foldable, Functor, Ord)

instance (Show a) => Show (One a) where
    show (One a) = "[" ++ show a ++ "]"

instance (Show a) => Show (Two a) where
    show (Two (a, b)) = "[" ++ show a ++ show b ++ "]"

instance (Show a) => Show (Three a) where
    show (Three (a, b, c)) = "[" ++ show a ++ show b ++ show c ++ "]"

instance Functor Three where
    f `fmap` (Three (a, b, c)) = Three (f a, f b, f c)

instance Foldable Three where
    f `foldMap` (Three (a, b, c)) = f a `mappend` f b `mappend` f c

class (Foldable a, Applicative a) => Composite a where
    compose     :: a [b] -> [a b]
    -- The latter should be more general than compose,
    -- working for any Applicative functor.
    -- Question: is it any standard operation for two applicatives?
    -- It seems that yes, then we wouldn't need this class at all.
    composeA :: (Applicative f) => a (f b) -> f (a b)
    
askAtomicOne :: AtomicQuestion -> OneSystem -> Bool
askAtomicOne (AtomicQuestion a alpha) (OneSystem p) = case Map.lookup a p of
        Just x  -> x == alpha
        Nothing -> False
askAtomicOne Null _          = False
askAtomicOne Trivial _       = True

askQA :: (Foldable a, Applicative a) => Question (a AtomicQuestion) -> a OneSystem -> Bool
askQA (Primitive a) p = and $ liftA2 askAtomicOne a p 
askQA (OPlus a b) p   = askQA a p || askQA b p 

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

instance Applicative Three where
    pure a = Three (a, a, a)
    (Three (f, g, h)) <*> (Three (a, b, c)) = Three (f a, g b, h c)

instance Composite One where
    compose (One qs) = map One qs
    composeA (One a) = fmap One a

instance Composite Two where
    compose (Two (ps, qs)) = [Two (p, q) | p <- ps, q <- qs]
    composeA (Two (ps, qs)) = curry Two <$> ps <*> qs

instance Composite Three where
    compose (Three (ps, qs, rs)) = [Three (p, q, r) | p <- ps, q <- qs, r <- rs]
    composeA (Three (ps, qs, rs)) = (\p q r -> Three (p, q, r)) <$> ps <*> qs <*> rs

atomicQuestionsOf :: Observable -> [AtomicQuestion]
atomicQuestionsOf obs = map (AtomicQuestion $ name obs) $ domain obs

phaseSubset :: (Composite a) => PhaseSpace (a OneSystem) -> Question (a AtomicQuestion) -> Set (a OneSystem)
phaseSubset (PhaseSpace points) q = Set.filter (askQA q) points
-- phaseSubset (PhaseSpace points) q = Set.filter (askQuestion q) points

boxWorldAtomicQs :: (Composite a) => a [Observable] -> [Question (a AtomicQuestion)]
boxWorldAtomicQs obs = map Primitive . compose . fmap atomsOf $ obs
    where
        atomsOf = concat . map atomicQuestionsOf 

boxWorldLogic :: (Composite c, Ord (c OneSystem)) 
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
