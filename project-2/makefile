all: interpreter

interpreter: $(wildcard src/**/*.hs)
	cabal build
	cp dist-newstyle/build/*/ghc-9.0.2/lager-lang-0.1.0.0/x/lager-lang/build/lager-lang/lager-lang interpreter

clean:
	cabal clean
	rm -f interpreter

.PHONY: all clean
