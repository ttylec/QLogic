GHCOPTS=-O3 -threaded -rtsopts -XBangPatterns -fforce-recomp
GHCDEBUG= -auto-all -caf-all -prof

all: effi bwo

debug: effi-debug bwo-debug

effi: effi.hs qlogic.hs examples.hs QLogic/boxproduct.hs QLogic/boxworld.hs
	ghc $(GHCOPTS) -o effi effi.hs

bwo: bwo.hs qlogic.hs QLogic/boxproduct.hs QLogic/boxworld.hs
	ghc $(GHCOPTS) -o bwo bwo.hs

effi-debug: effi.hs qlogic.hs examples.hs QLogic/boxproduct.hs QLogic/boxworld.hs
	ghc $(GHCOPTS) $(GHCDEBUG) -o effi effi.hs

bwo-debug: bwo.hs qlogic.hs QLogic/boxproduct.hs QLogic/boxworld.hs
	ghc $(GHCOPTS) $(GHCDEBUG) -o bwo bwo.hs

clean:
	rm bwo effi
