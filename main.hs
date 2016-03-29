module Main (main) where
import Lexer (get_tokens)
import Parser (parseAscii)
import Ast

f :: String -> String
f = (flip (++) $ "\n") . show . parseAscii . get_tokens

main = interact f
