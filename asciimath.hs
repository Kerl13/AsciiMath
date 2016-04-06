module Asciimath (readAscii, writeTeX, compile) where
import Lexer (get_tokens)
import Parser (parseAscii)
import Passes (matrix)
import TeXWriter (writeTeX)
import Ast

readAscii :: String -> Code
readAscii = matrix . parseAscii . get_tokens

compile :: String -> String
compile = writeTeX . readAscii

