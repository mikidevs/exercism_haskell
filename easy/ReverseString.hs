reverseString :: String -> String
reverseString = reverseStringRec ""

reverseStringRec :: String -> String -> String
reverseStringRec acc "" = acc
reverseStringRec acc (c:rest) = reverseStringRec (c:acc) rest