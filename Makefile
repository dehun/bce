make:
	cabal install --only-dependencies
	cabal configure --enable-tests
	cabal build
test :
	cabal install --only-dependencies
	cabal configure --enable-tests
	cabal build test && ./dist/build/test/test --color -j8 +RTS -N8 -RTS
