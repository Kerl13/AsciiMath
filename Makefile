BIN=asciimath
COMP_OPT=-O2

GENERATED=lexer.hs parser.hs
MODULES=lexer ast parser main

all: $(GENERATED)
	ghc $(COMP_OPT) $(MODULES) -o $(BIN)

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
