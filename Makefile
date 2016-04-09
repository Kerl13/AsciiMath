cabal:
	cabal install --only-dependencies
	cabal configure --enable-optimization
	cabal build
	cp dist/build/asciimath/asciimath .
	cp dist/build/pandoc-asciimath/pandoc-asciimath .

stack:
	stack install

clean:
	cabal clean
	stack clean
	rm -rf dist
	rm -f pandoc-asciimath
	rm -f asciimath
