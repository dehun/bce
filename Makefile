.PHONY: all test clean

configure:
	cabal install --only-dependencies
	cabal configure --enable-tests

make: configure
	cabal build
test : configure
	cabal build test && ./dist/build/test/test --color -j8 +RTS -N2 -RTS
clean:
	cabal clean
