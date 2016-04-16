module AsciiMath (readAscii, writeTeX, compile, run) where
import Control.Exception (throw)
import Lexer (get_tokens, LexicalError(..))
import Parser (parseAscii)
import Passes (matrix)
import TeXWriter (writeTeX)
import Ast

apL :: (a -> Either e b) -> Either e a -> Either e b
apL f (Right x) = f x
apL _ (Left err) = Left err

returnE :: a -> Either LexicalError a
returnE x = Right x

readAscii :: String -> Either LexicalError Code
readAscii s = apL (returnE . matrix) $ apL parseAscii $ get_tokens s
  
compile :: String -> Either LexicalError String
compile s = case readAscii s of
  Right x -> Right $ writeTeX x
  Left e -> Left e

run :: String -> String
run s = case compile s of
  Right txt -> txt
  Left e -> throw e
