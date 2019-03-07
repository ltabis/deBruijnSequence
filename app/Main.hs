module Main where

import System.Exit
import System.IO
import DeBruijn
import Parser

main :: IO ()
main = do a <- processArgs
          if a == []
          then displayHelp
          else if length (a) <= 4
               then compareArguments a $ isNum (a!!0)
               else displayHelp
