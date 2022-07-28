module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import GHC.Conc (retry)


main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzel (fmap toLower word)
  runGame puzzle

-- type WordList = [String]
newtype WordList = WordList [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

maxWordLength :: Int
maxWordLength = 9

minWordLength :: Int
minWordLength = 5 

gameWords :: IO WordList
gameWords = do
  (WordList al) <- allWords
  return $ WordList (filter wordLength al)
  where 
    wordLength w =
      let l = length (w :: String)
      in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, l)
  return $ wl !! randomIndex
  where
    l = length wl - 1

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = "The puzzle word is: "
    ++ intersperse ' ' (fmap renderCharPuzzle discovered)
    ++ " Guessed so far: " ++ guessed

renderCharPuzzle :: Maybe Char -> Char
renderCharPuzzle = fromMaybe '_'

freshPuzzel :: String -> Puzzle
freshPuzzel word = Puzzle word discovered []
  where discovered = fmap (const Nothing) word
  
charInWord :: Char -> Puzzle -> Bool
charInWord c (Puzzle word _ _) = c `elem` word

alreadyGuessed :: Char -> Puzzle -> Bool
alreadyGuessed c (Puzzle _ _ guessed) = c `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered guessed) c =
  Puzzle word newDiscovered newGuessed
  where zipper ch wordChar guessedChar =
          if ch == wordChar
          then Just ch 
          else guessedChar
        newDiscovered = zipWith (zipper c) word discovered
        newGuessed = 
          if c `elem` word
          then guessed 
          else c:guessed

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle c = do
  putStrLn $ "You guess a character is " ++ [c]
  case (charInWord c puzzle, alreadyGuessed c puzzle) of
    (True, _) -> do 
      putStrLn "The character is in the word." 
      return (fillInCharacter puzzle c)
    (_, True) -> do
      putStrLn "You are already guessec the character "
      return puzzle
    (False, _) -> do
      putStrLn "You geussed wrong. Try again"
      return (fillInCharacter puzzle c)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) = do
  if length guessed > 7 then 
    do putStrLn "You loose."
       putStrLn $ "The word was: " ++ word
       exitSuccess
    else return ()

gameWin :: Puzzle -> IO () 
gameWin (Puzzle _ discovered _) = 
  if all isJust discovered then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a char: "
  ch <- getLine
  case ch of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must\
                  \ be a single character"


    
