.PHONY: all test clean make

configure:
	cabal install --only-dependencies
	cabal configure --enable-tests

all: configure
	cabal build
	cabal build bcedaemon
	cabal build bcecli

test : configure
	cabal build test && ./dist/build/test/test --color -j8 +RTS -N2 -RTS

clean:
	cabal clean

make: all
