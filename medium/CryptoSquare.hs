module CryptoSquare (encode) where
import Data.Char (isAlphaNum, toLower)
import Data.Function ((&))
import Data.List (transpose)
import Text.Printf (printf)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

encode :: String -> String
encode xs =
  normalised
    & chunksOf c
    & transpose
    & map (printf (concat ["%-", show r, "s"]))
    & unwords
  where
    normalised =
      xs 
      & filter isAlphaNum
      & map toLower 

    (c, r) = findRect $ length normalised

findRect :: Int -> (Int, Int)
findRect len =
  if r ^ 2 < len then (r + 1, r) else (r, r)
  where r = (+1) $ length $ takeWhile (<= len) [x^2 + x | x <- [1..]]
