module Main (main) where
import Lexer (get_tokens)
import Parser (parseAscii)
import TeXWriter (writeTeX)

endl :: String -> String
endl = (flip (++)) "\n"

run :: String -> String
run = endl . writeTeX . parseAscii . get_tokens

main = interact run
