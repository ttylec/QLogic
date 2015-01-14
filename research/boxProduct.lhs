> import Data.QLogic
> import Data.QLogic.Examples
> import Data.QLogic.BoxProduct

Crucial properties 
==================


Basic example
=============

Here is a partialy ordered set of 2-2-box world:

> boxWorld22Questions = boxQuestions lanternLogic lanternLogic 
> boxWorld22Poset = boxProduct' lanternLogic lanternLogic boxWorld22Questions
> (@+@) = freePlus (lanternLogic, lanternLogic) boxWorld22Poset

> boxWorld22 = boxAtomicProduct lanternLogic lanternLogic

