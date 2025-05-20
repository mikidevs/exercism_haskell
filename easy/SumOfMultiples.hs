module SumOfMultiples (sumOfMultiples) where
import Data.List (nub)

multiples _ 0 = []
multiples limit m = takeWhile (<limit) (map (*m) [1..])

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $ nub $ concatMap (multiples limit) factors 