GHCOPTS=-O3 -threaded -rtsopts -XBangPatterns -fforce-recomp
GHCDEBUG= -auto-all -caf-all -prof

LIBDEPS=QLogic.hs QLogic/BoxProduct.hs QLogic/BoxWorld.hs

all: effi bwo

debug: effi-debug bwo-debug

effi: effi.hs $(LIBDEPS) 
	ghc $(GHCOPTS) -o effi effi.hs

bwo: bwo.hs $(LIBDEPS)
	ghc $(GHCOPTS) -o bwo bwo.hs

effi-debug: effi.hs $(LIBDEPS)
	ghc $(GHCOPTS) $(GHCDEBUG) -o effi effi.hs

bwo-debug: bwo.hs $(LIBDEPS)
	ghc $(GHCOPTS) $(GHCDEBUG) -o bwo bwo.hs

clean:
	rm bwo effi
