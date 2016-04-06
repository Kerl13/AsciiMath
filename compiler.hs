module Main (main) where
import Asciimath (compile)

endl :: String -> String
endl = (flip (++)) "\n"

main :: IO ()
main = interact (endl . compile)
