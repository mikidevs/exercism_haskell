module Prime (nth) where

isPrime :: Integer -> Bool
isPrime n =
  let m = n `div` 2 in
    m - 1 == toInteger (length (takeWhile not [n `mod` x == 0 | x <- [2..m]]))

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = 
  Just $ last $ take n [x | x <- [2..], isPrime x]