module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = collatzRec n 0

collatzRec n acc
  | n < 1 = Nothing
  | n == 1 = Just acc
  | even n = collatzRec (n `div` 2) (acc + 1)
  | otherwise = collatzRec (3 * n + 1) (acc + 1)
