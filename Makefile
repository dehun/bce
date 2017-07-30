.PHONY: all test clean make

configure:
	cabal install --only-dependencies
	cabal configure --enable-tests

make: configure
	cabal build
test : configure
	cabal build test && ./dist/build/test/test --color -j8 +RTS -N8 -RTS
clean:
	cabal clean
