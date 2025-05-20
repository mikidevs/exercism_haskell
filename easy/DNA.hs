module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = case toRNARec xs "" of
  Right x -> Right (reverse x)
  Left x -> Left x

toRNARec :: String -> String -> Either Char String
toRNARec "" acc = Right acc
toRNARec (x : xs) acc =
  case x of
    'G' -> toRNARec xs ('C' : acc) 
    'C' -> toRNARec xs ('G' : acc) 
    'T' -> toRNARec xs ('A' : acc) 
    'A' -> toRNARec xs ('U' : acc) 
    _ -> Left x