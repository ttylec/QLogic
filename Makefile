GHCOPTS=-O3 -threaded -rtsopts -XBangPatterns -fforce-recomp -auto-all -caf-all -prof

all: effi

effi: effi.hs poset.hs
	ghc $(GHCOPTS) -o effi effi.hs
