module Ciphers where

import Data.Char

alphabetMod:: Int
alphabetMod = ord 'z' - ord 'a' + 1

encryptChar :: (Char -> Char) -> Char -> Char
encryptChar cipher ch = 
  if isAlpha ch 
  then cipher ch
  else ch

shiftChar :: Char -> Int
shiftChar ch = 
  if isUpper ch
  then ord 'A'
  else ord 'a'
 


caesar :: Int -> Char -> Char
caesar key ch = encryptChar encrypt ch
  where
    shift = shiftChar ch
    encrypt ch = chr $ mod (ord ch - shift + key) alphabetMod + shift

encryptCaesar :: Int -> String -> String
encryptCaesar key = map (caesar key)

decryptCaesar :: Int -> String -> String
decryptCaesar key = map (caesar (-key))

charToKey :: Char -> Int
charToKey ch = ord ch - shiftChar ch

encryptVigenere :: String -> String -> String
encryptVigenere key text = zipBy f isAlpha text (cycle key)
  where
    f ch key = chr (mod (ord ch - shiftChar ch + ord key - shiftChar key) alphabetMod + shiftChar ch)
             
              
zipBy :: (a -> a -> a) -> (a -> Bool) -> [a] -> [a] -> [a]
zipBy f p x y = go f p x y
  where go _ _ [] _ = []
        go _ _ _ [] = []
        go f p x y = 
          if p (head x) 
          then f (head x) (head y) : go f p (tail x) (tail y)
          else head x : go f p (tail x) y
        
encVigenere :: IO ()
encVigenere = do
  putStrLn "Enter text: "
  text <- getLine
  putStrLn "Enter key: "
  key <- getLine
  putStrLn $ encryptVigenere key text
