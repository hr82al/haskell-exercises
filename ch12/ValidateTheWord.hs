module ValidateTheWord where

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel ch = ch `elem` vowels

mkWord :: String -> Maybe Word'
mkWord word = 
  let 
    vowels = length . filter isVowel $ word
    consonants = length word - vowels
  in 
    if vowels > consonants
    then Nothing
    else Just $ Word' word
