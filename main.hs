module Main (main) where
import Lexer (get_tokens)
import Ast


f :: String -> String
f = (flip (++) $ "\n") . show . get_tokens

main = interact f
