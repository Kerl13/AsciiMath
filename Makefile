BIN=asciimathreader
GENERATED=lexer.hs
MODULES=lexer ast main

all: $(GENERATED)
	ghc $(MODULES) -o $(BIN)

.SUFFIXES: .hs .x .y

%.hs: %.x
	alex $< 

%.hs: %.y
	happy $<

clean:
	rm -f *.o *.hi
	rm -f $(GENERATED)
	rm -f $(BIN)

exec: $(BIN)
	./$(BIN)
