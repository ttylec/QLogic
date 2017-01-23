module Main (main) where

import Data.List
import System.Environment

import QStructures
import QStructures.Concrete
import QStructures.Hilbert
import BoxModels
import BoxModels.SetRepresentation
import BoxModels.HilbertRepresentation


-- model = let x = binaryO 'X'
--             y = binaryO 'Y'
--             z = binaryO 'Z'
--         in
--           two [x, y, z] [x, y, z]

model = let x = binaryO 'X'
            y = binaryO 'Y'
        in
          two [x, y] [x, y]

main :: IO ()
main = do
  let
    -- representation = phaseSubset phasespace
    -- constructor = ConcreteOMP gamma
    -- phasespace@(PhaseSpace gamma) = phaseSpace model
    representation = questionRepr
    constructor = HilbertOMP 4
    atoms = map representation $ boxAtoms model
    propositions = generateSubStructure . constructor $ atoms

  print . length $ atoms
  print . length $ propositions
  print . length . nubBy (~==) $ propositions