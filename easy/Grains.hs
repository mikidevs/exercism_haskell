module Grains (square, total) where
import Data.Maybe as M

square :: Integer -> Maybe Integer
square n
  | n < 1 = Nothing
  | n > 64 = Nothing
  | otherwise = Just $ 2 ^ (n - 1)

total :: Integer
total = sum [M.fromMaybe 0 (square x) | x <- [1..64]]