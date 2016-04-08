module Main (main) where
import AsciiMath (compile)

endl :: String -> String
endl = (flip (++)) "\n"

main :: IO ()
main = interact (endl . compile)
