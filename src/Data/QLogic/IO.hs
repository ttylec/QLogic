{-# LANGUAGE GADTs, BangPatterns #-}

module Data.QLogic.IO (writePoset, writeQLogic, Repr, repr) where

import Data.List
import qualified Data.Map.Strict as Map
import Data.QLogic
import Data.Relation
import Data.Poset.Internals
import Data.QLogic.BoxProduct

class Repr a where
        repr :: a -> String

instance (Repr a) => Repr (Equiv a) where
        repr = repr . equivRepr

instance (Repr a, Repr b) => Repr (FreeProduct a b) where
        repr (FreeProd a b) = repr a ++ repr b
        repr (FreePlus a as) = repr a ++ "PLUS" ++ repr as

instance (Repr a, Repr b) => Repr (a, b) where
        repr (a, b) = "(" ++ repr a ++ ", " ++ repr b ++ ")"

instance Repr a => Repr [a] where
        repr a = "[" ++ (intercalate ", " $ map repr a) ++ "]"

writeQLogic :: (Ord a, Repr a, Repr p) => String -> QLogic p a -> String
writeQLogic name (QLogic poset omap min max) = writePoset name poset ++ somap ++ decl ++ "\n"
    where
        nameOMap = name ++ "OCmpl"
        namePoset = name ++ "Poset"
        somap = unlines $ map (\x -> nameOMap ++ " " ++ (repr x) ++ " = " ++ (repr $ omap x)) $ elementsOf poset 
        decl = name ++ "Qlogic = QLogic " ++ namePoset ++ " " ++ nameOMap ++ " " ++ (repr min) ++ " " ++ (repr max)

writePoset:: (Ord a, Repr a, POrdStruct p a) => String -> p -> String
writePoset name poset = header ++ datatype ++ pord_instance ++ "\n"
    where
        rels = map repr $ elementsOf poset
        datatype = "data " ++ name ++ " = " ++ (intercalate " | " rels) ++ " deriving (Bounded, Enum, Eq, Ord, Show)\n"
        pord_instance = "instance POrd " ++ name ++ " where\n" ++ pords ++ "    _ .<=. _ = False\n"
        pords = unlines $ map inRelationTo $ elementsOf poset
        inRelationTo x = unlines $ map (\y -> "    " ++ (repr x) ++ " .<=. " ++ (repr y) ++ " = True ") $ geEqThan poset x
        header = "module " ++ name ++ " where\n\nimport Data.Poset\n\n"
