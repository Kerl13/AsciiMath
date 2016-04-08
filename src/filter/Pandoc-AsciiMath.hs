import AsciiMath (compile)
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter asciimath
    where asciimath (Math t s) = Math t $ compile s
          asciimath x = x
