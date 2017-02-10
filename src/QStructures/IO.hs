{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module QStructures.IO where

import Data.Ratio
import Data.Graph
import qualified Data.Text as T
import qualified Data.Map as Map


class MathematicaForm a where
  mform :: a -> T.Text

instance MathematicaForm Int where
  mform = T.pack . show

instance MathematicaForm Double where
  mform = T.pack . show

instance Show a => MathematicaForm (Ratio a) where
  mform x = (T.pack . show . numerator $ x) `T.append`
    "/" `T.append`
    (T.pack . show . denominator $ x)

instance MathematicaForm a => MathematicaForm [a] where
  mform m = "{" `T.append` (T.intercalate ", " . map mform $ m)
    `T.append` "}"

instance MathematicaForm T.Text where
  mform = id

instance MathematicaForm Graph where
  mform = mform . map fmtEdge . edges
    where
      fmtEdge (a, b) = (T.pack $ show a) `T.append` " -> "
        `T.append` (T.pack $ show b)
