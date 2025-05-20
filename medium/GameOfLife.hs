module GameOfLife (tick) where
import Data.Maybe (isJust, fromMaybe)

type Coord = (Int, Int)

type Grid a = [[a]]

neighbours :: Grid Int -> Coord -> [Int]
neighbours grid (x, y) =
  map (fromMaybe 0) $ filter isJust excludeSelf
  where
    cells = [getCell grid (x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
    excludeSelf = take 4 cells ++ drop 5 cells

getCell :: Grid a -> Coord -> Maybe a
getCell grid (x, y) =
  if y >= 0 && y < length grid && x >= 0 && x < length (head grid)
    then Just ((grid !! y) !! x)
    else Nothing

rule :: Grid Int -> Coord -> Int -> Int
rule grid coord val =
  case (sum (neighbours grid coord), val) of
    (2, 1) -> 1
    (3, 1) -> 1
    (3, 0) -> 1
    (_, _) -> 0

indexMap :: (Int -> a -> b) -> [a] -> [b]
indexMap f = zipWith f [0..]

tick :: Grid Int -> Grid Int
tick grid =
  indexMap (\j y -> indexMap (\i x -> rule grid (i, j) x) y) grid