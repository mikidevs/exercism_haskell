module Luhn (isValid) where
import Data.Char (isNumber)

circDouble :: Int -> Int
circDouble n =
  if x > 9 then x - 9 else x
  where x = n * 2

isValid :: String -> Bool
isValid n =
    (length parsed >= 2) && (
      let len = length parsed
          indexed = zip [len, (len - 1)..0] parsed -- Reverse index for right to left
          summed = sum [if even i then circDouble x else x | (i, x) <- indexed] in
            summed `mod` 10 == 0)
    where parsed = map ((read :: String -> Int) . (: [])) (filter isNumber n)