module Main where

import Dogy (sayDogy)
import Hello
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter the name: "
  name <- getLine
  sayHello name
  sayDogy
