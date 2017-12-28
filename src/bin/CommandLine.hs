module Main (main) where

import Control.Exception as E
import AsciiMath

main :: IO ()
main = E.catch (interact $ (++ "\n") . run) printAndExit
