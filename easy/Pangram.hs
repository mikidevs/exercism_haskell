module Pangram (isPangram) where
import Data.Char (ord, toLower, isSpace, isAscii, isLetter)
import Data.List (nub)

isPangram :: String -> Bool
isPangram str =
  let letters = nub [toLower c | c <- str, isLetter c && isAscii c]
  in length letters == 26