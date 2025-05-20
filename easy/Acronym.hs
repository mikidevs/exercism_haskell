module Acronym (abbreviate) where
import Data.Char (isLetter, isUpper, isLower, toUpper)
import Data.Bool (bool)

capFirst "" = ""
capFirst (x : xs) =
  toUpper x : xs

breakWords w =
  let (f, s) = break (=='-') w in
    capFirst f ++ (capFirst . drop 1) s

letterOrSep c = c == '-' || isLetter c

abbreviate :: String -> String
abbreviate xs =
  concatMap (keepUpper . breakWords . filter letterOrSep) $ words xs

keepUpper :: String -> String
keepUpper t = case (all isUpper t, all isLower t) of
  (True, False) -> take 1 t
  (False, True) -> map toUpper $ take 1 t
  _ -> filter isUpper t