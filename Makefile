all:
	cabal configure
	cabal build
	ln -s dist/build/asciimath/asciimath asciimath
	ln -s dist/build/pandoc-asciimath/pandoc-asciimath pandoc-asciimath

clean:
	rm -rf dist
	rm -f pandoc-asciimath
	rm -f asciimath
