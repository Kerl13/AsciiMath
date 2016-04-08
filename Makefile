all:
	cabal configure
	cabal build

clean:
	rm -rf dist
