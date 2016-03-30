module Main (main) where
import Lexer (get_tokens)
import Parser (parseAscii)
import TeXWriter (writeTeX)

endl :: String -> String
endl = (flip (++)) "\n"

f :: String -> String
f = endl . writeTeX . parseAscii . get_tokens

main = interact f
