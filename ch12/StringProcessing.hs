module StringProcessing where
import Data.Maybe (isNothing)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe word = Just word

text = "the cow loves us"

replaceThe :: String -> String
replaceThe = unwords . go . words 
  where
    go [] = []
    go (word:words) = 
      (if isNothing . notThe $ word
      then "a"
      else word) : go words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    check word1 word2 = if isNothing (notThe word1) && head word2 `elem` "aeiou"
      then 1 
      else 0
    go [] = 0
    go [_] = 0
    go (word1:word2:words) = check word1 word2 + go (word2:words)
  
isVowel :: Char -> Bool
isVowel ch = ch `elem` "aeijou"

getVowels :: String -> String
getVowels = filter isVowel

countVowels :: String -> Integer
countVowels = fromIntegral . length . getVowels
