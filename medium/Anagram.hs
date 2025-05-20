module Anagram (anagramsFor) where
import qualified Data.List as L
import Data.Char (toLower)

anagramOf :: String -> String -> Bool
anagramOf candidate target =
  let lCandidate = map toLower candidate in
  let lTarget = map toLower target in
    length candidate == length target && ((lCandidate /= lTarget) && (L.sort lCandidate L.\\ L.sort lTarget) == "")

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = L.filter (anagramOf xs)