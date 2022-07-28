module Main where

import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (stringToMorse, morseToChar)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hGetLine, hIsEOF, stdin)


convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess

  -- otherwise, procced
  line <- hGetLine stdin
  convertLine line
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just str)
          -> putStr (intercalate " " str)
        Nothing
          -> do
            putStrLn $ "ERROR: " ++ line
            exitFailure

converFromMorse :: IO ()
converFromMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess

  -- otherwise, procced
  line <- hGetLine stdin
  convertLine line

  where
    convertLine line = do
      let decoded :: Maybe String
          decoded =
            traverse morseToChar (words line)
      case decoded of
        (Just s) -> putStrLn s 
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure


main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] -> 
      case arg of
        "from" -> converFromMorse
        "to"   -> convertToMorse
        _      -> argError
    _ -> argError

  where argError = do
          putStrLn "Please specify the\
                  \ first argument\
                  \ as being 'form' or\
                  \ 'to' morse,\
                  \ such as: morse to"
          exitFailure
