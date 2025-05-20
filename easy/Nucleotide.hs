module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as M
import           Data.Map (Map)
import Data.List (sort, nub, (\\), foldr)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  case ((\\) . sort . nub) xs "ACGT" of
    "" -> 
      let f c = M.insertWith (+) (keyOf c) 1 in
      Right $ foldr f M.empty xs
    _ -> Left "error"

keyOf 'A' = A
keyOf 'C' = C
keyOf 'G' = G
keyOf 'T' = T