module Main (main) where

import Control.Exception as E
import Text.Pandoc.JSON
import AsciiMath


main :: IO ()
main = E.catch (toJSONFilter asciimath) printAndExit
  where asciimath (Math t s) = Math t (run s)
        asciimath x = x
