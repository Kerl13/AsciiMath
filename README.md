# AsciiMath

AsciiMath is a compiler from the amazing [AsciiMath](http://asciimath.org/)
language to LaTeX.

The aim of that project is to provide a Haskell Library and a command line tool
making possible to use asciimath everywhere, for example in interaction with
[pandoc](http://pandoc.org/) or for preprocessing on your
[ocsigen](http://ocsigen.org/) website.

## Install tips

To get this tool, you just have to clone this repository and run `cabal install`
in the cloned folder to install the dependencies and then `make` to compile the
project. That's it !

If you have trouble with cabal, you can install the dependencies manually by
running  
`cabal update && cabal install alex && cabal install happy`

## Usage

The executable `asciimath` reads AsciiMath code from its standard input, compile
it and prints the resulting LaTeX code on its standard output. For example :

    > echo "sum_(i=1)^n i^3=((n(n+1))/2)^2" | ./asciimath
    \sum_{i=1}^{n}i^{3}=\left(\frac{n\left(n+1\right)}{2}\right)^{2}

The `Lexer`, `Parser` and `TeXWriter` modules also provide three functions :
* `get_tokens :: String -> [Lexer.Token]`
* `parseAscii :: [Lexer.Token] -> Ast.Code`
* `writeTeX :: Ast.Code -> String`

which can be used to make a [pandoc filter](http://pandoc.org/scripting.html),
play with the AST or anything else.

## TODO

* Locating parsing errors
* '\ ' is not handled
* Matrices are not implemented yet

## Grammar

Here is the grammar used to parse AsciiMath. It is a little different from the
original one defined [here](http://asciimath.org/#grammar) but I think the
changes I made respect the original idea.


c ::= `[a-zA-Z]` | _numbers_ | _greek letters_ | _standard functions_ | _other
symbols_ (see [list](http://asciimath.org/#syntax))

u ::= `sqrt` | `text` | `bb` | `bbb` | `cc` | `tt` | `fr` | `sf`
| `hat` | `bar` | `ul` | `vec` | `dot` | `ddot`

b ::= `frac` | `root` | `stackrel`

l ::= `(` | `[` | `{` | `(:` | `{:`

r ::= `)` | `]` | `}` | `:)` | `:}`

S ::= c | l Code r | u S | b S S | `"` _any text_ `"`

E ::= S | S `/` S | S `_` S | S `^` S | S `_` S `^` S

Code ::= [ E ]


## Rendering rules

* The constants are converted to their LaTeX equivalent
* `sqrt`, `hat`, `bar`, `vec`, `dot` and `ddot` are prefixed with a `\`
* `text` and `ul` correspond to the `\textrm` and `underline` functions
* `bb`, `bbb`, `cc`, `tt`, `fr` and `sf` correspond to the `\textbf`,
  `\mathbb`, `\mathcal`, `\texttt`, `\mathfrak` and `\textsf` functions
* `frac` is rendered as a fraction, `root n x` as the `n`-th root of `x` and
  `stackrel x y` displays `x` upon `y`
* Left and right delimiters are preceded by the `\left` and `\right` commands to
  be well-sized. `(:` and `:)` are chevrons. `{:` and `:}` are invisible
  delimiters. The others are rendered as expected
* Any text placed between a pair of `"` is rendered in the same font as normal
  text.
* `/` stands for a fraction. The `_` and `^` tokens have the same behaviour as
  in LaTeX but the subscript must be place before the superscript if they are
  both present
* Matrices can be rendered as explained [here](http://asciimath.org/#syntax)
  **[not implemented yet]**


