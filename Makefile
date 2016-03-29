BIN=asciimath
GENERATED=lexer.hs parser.hs
MODULES=lexer ast parser main

all: $(GENERATED)
	ghc $(MODULES) -o $(BIN)

.SUFFIXES: .hs .x .y

%.hs: %.x
	alex $< 

%.hs: %.y
	happy $<

clean:
	rm -f *.o *.hi
	rm -f parser.info
	rm -f $(GENERATED)
	rm -f $(BIN)

exec: $(BIN)
	./$(BIN)
