BIN=asciimath
COMP_OPT=

GENERATED=lexer.hs parser.hs
MODULES=lexer ast parser texwriter

all: compiler filter

base: $(GENERATED)

filter: base
	ghc $(COMP_OPT) $(MODULES) pandoc-asciimath -o pandoc-asciimath 

compiler: base
	ghc $(COMP_OPT) $(MODULES) asciimath -o $(BIN)

.SUFFIXES: .hs .x .y

%.hs: %.x
	alex -g $< 

%.hs: %.y
	happy -g $<

clean:
	rm -f *.o *.hi
	rm -f parser.info
	rm -f $(GENERATED)
	rm -f $(BIN)

