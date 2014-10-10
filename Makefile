GHCOPTS=-O3 -threaded -rtsopts -XBangPatterns -fforce-recomp -auto-all -caf-all -prof

all: effi bwo

effi: effi.hs qlogic.hs examples.hs
	ghc $(GHCOPTS) -o effi effi.hs

bwo: bwo.hs qlogic.hs QLogic/boxproduct.hs QLogic/boxworld.hs
	ghc $(GHCOPTS) -o bwo bwo.hs
