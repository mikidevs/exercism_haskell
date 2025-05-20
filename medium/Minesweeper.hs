module Minesweeper (annotate) where
import Data.Maybe (fromMaybe)

type Coord = (Int, Int)
type Grid = [String]

neighbours :: Grid -> Coord -> [Char]
neighbours grid (a, b) =
  let c = [(a + x, b + y) | x <- [-1, 0, 1], y <- [-1, 0, 1]]
      coords = take 4 c ++ drop 5 c in
      map (fromMaybe ' ' . getCell grid) coords

intToChar :: Int -> Char
intToChar n = toEnum (n + fromEnum '0')

transform :: Grid -> Coord -> Char
transform grid coord = 
  case getCell grid coord of
    Just ' ' -> intToChar $ (length . filter (== '*')) (neighbours grid coord)
    Just '*' -> '*'
    _ -> ' '

getCell :: Grid -> Coord -> Maybe Char
getCell grid (x, y) =
  if y >= 0 && y < length grid && x >= 0 && x < length (head grid)
    then Just ((grid !! y) !! x)
    else Nothing

annotate :: Grid -> Grid
annotate board = error "TODO"
