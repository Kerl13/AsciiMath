cabal: deps
	cabal configure --enable-optimization
	cabal build
	cp dist/build/asciimath/asciimath .
	cp dist/build/pandoc-asciimath/pandoc-asciimath .

filter-only: deps
	cabal configure --enable-optimization
	cabal build pandoc-asciimath
	cp dist/build/pandoc-asciimath/pandoc-asciimath .

deps:
	cabal install --only-dependencies

stack:
	stack install

clean:
	cabal clean
	stack clean
	rm -rf dist
	rm -f pandoc-asciimath
	rm -f asciimath
