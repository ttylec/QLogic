{-# LANGUAGE GADTs #-} 
module Data.QLogic.BoxWorld where

import Data.List
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

data Observable = Observable { name   :: Char
                             , domain :: [Int] } deriving (Eq, Ord, Show)

type OneSystem = Map Char Int

newtype PhaseSpace a = PhaseSpace (Set a) deriving (Show)

phasePoints :: PhaseSpace a -> [a]
phasePoints (PhaseSpace a) = Set.toList a

phasePointsSet :: PhaseSpace a -> Set a
phasePointsSet (PhaseSpace a) = a

phaseSpace :: (Composite a, Ord (a OneSystem)) => a [Observable] -> PhaseSpace (a OneSystem)
phaseSpace obs = PhaseSpace $ Set.fromList . compose . fmap phaseSpace1' $ obs

phaseSpace1' :: [Observable] -> [OneSystem]
phaseSpace1' obs = map Map.fromList $ tuples [[(name o, k) | k <- domain o] | o <- obs]
    where
        tuples (v:[]) = map (:[]) v
        tuples (v:vs) = [p:ps | p <- v, ps <- tuples vs]

data AtomicQuestion = AtomicQuestion Char Int
data Question a = Primitive a | OPlus (Question a) (Question a)

instance Show AtomicQuestion where
    show (AtomicQuestion a alpha) = a:show alpha

instance (Show a) => Show (Question a) where
   show (Primitive a) = show a
   show (OPlus a b) = show a ++ "+" ++ show b

infixl 4 <+>
(<+>) = OPlus

newtype One a = One a deriving (Eq, Ord)
newtype Two a = Two (a, a) deriving (Eq, Ord)

instance (Show a) => Show (One a) where
    show (One a) = "[" ++ show a ++ "]"

instance (Show a) => Show (Two a) where
    show (Two (a, b)) = "[" ++ show a ++ show b ++ "]"

class (Functor a) => Composite a where
    -- This can be generalized to applicative functor,
    -- but I need to learn a about it (esp. how to write
    -- instance for Composite Two.
    compose     :: a [b] -> [a b]
    -- Similarly, askAtomic and askQuestion should be
    -- expressible by some good combinators of Composite class
    askAtomic   :: a AtomicQuestion -> a OneSystem -> Bool
    askQuestion :: Question (a AtomicQuestion) -> a OneSystem -> Bool
    askQuestion (Primitive q) x = askAtomic q x
    askQuestion (OPlus p q) x = askQuestion p x || askQuestion q x
    -- In fact Composite class is probably some Monad, MonadPlus
    -- or sth like that. I have to read about these abstractions
    -- and implement it here.

instance Functor One where
    f `fmap` (One a) = One $ f a

instance Composite One where
    compose (One qs) = map One qs
    askAtomic (One (AtomicQuestion a alpha)) (One p) = case Map.lookup a p of
        Just x  -> x == alpha
        Nothing -> False

instance Functor Two where
    f `fmap` (Two (a, b)) = Two (f a, f b)

instance Composite Two where
    compose (Two (ps, qs)) = [Two (p, q) | p <- ps, q <- qs]
    askAtomic (Two (p, q)) (Two (x, y)) = askAtomic (One p) (One x) 
                                       && askAtomic (One q) (One y) 

atomicQuestionsOf :: Observable -> [AtomicQuestion]
atomicQuestionsOf obs = map (AtomicQuestion $ name obs) $ domain obs

phaseSubset :: (Composite a) => PhaseSpace (a OneSystem) -> Question (a AtomicQuestion) -> Set (a OneSystem)
phaseSubset (PhaseSpace points) q = Set.filter (askQuestion q) points

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
