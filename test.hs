module Test where

import qualified Data.Map.Strict as Map

testElements = [Zero, X0, X1, Y0, Y1, One]
testRelation = Map.fromList [(Zero, [Zero, X0, X1, Y0, Y1, One]), (X0, [X0, One]), (X1, [X1, One]), (Y0, [Y0, One]), (Y1, [Y1, One]), (One, [One])]
testPoset = Poset testElements (ListRel testRelation)
testOCmpl Zero = One
testOCmpl X0 = X1
testOCmpl X1 = X0
testOCmpl Y0 = Y1
testOCmpl Y1 = Y0
testOCmpl One = Zero
testQlogic = QLogic testPoset testOCmpl Zero One
