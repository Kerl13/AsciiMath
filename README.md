# AsciiMath

AsciiMath is a compiler from the amazing [asciimath](http://asciimath.org/)
language to LaTeX.

The aim of that project is to provide a Haskell Library and a command line tool
making possible to use asciimath everywhere, for example in interaction with
[pandoc](http://pandoc.org/scripting.html).

## Usage

The executable `asciimath` reads ASCIIMath code from its standard input, compile
it and prints the resulting LaTeX code on its standard output. For example :

    > echo "sum_(i=1)^n i^3=((n(n+1))/2)^2" | ./asciimath
    \sum_{i=1}^{n}i^{3}=\left(\frac{n\left(n+1\right)}{2}\right)^{2}

The `Lexer`, `Parser` and `TeXWriter` modules also provide three functions :
* `get_tokens :: String -> [Lexer.Token]`
* `parseAscii :: [Lexer.Token] -> Ast.Code`
* `writeTeX :: Ast.Code -> String`

which can be used to make a [pandoc](http://pandoc.org/scripting.html) filter,
or anything else.

## TODO

* Locating parsing errors
* '\ ' is not handled
* Matrices are not implemented yet


