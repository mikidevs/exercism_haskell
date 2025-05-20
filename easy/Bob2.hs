module Bob2 (responseFor) where
import qualified Data.Text as T
import           Data.Text (Text)
import Data.Char (isLetter, isUpper, isSpace)

responseFor :: Text -> Text
responseFor xs
  | shoutyQuestion = T.pack "Calm down, I know what I'm doing!"
  | question = T.pack "Sure."
  | shout = T.pack "Whoa, chill out!"
  | silence = T.pack "Fine. Be that way!"
  | otherwise = T.pack "Whatever."
  where
    question = isQuestion xs
    shout = isShouty xs
    shoutyQuestion = isShouty xs && isQuestion xs
    silence = isEmpty xs

-- isQuestion = maybe False ((== '?') . snd) . T.unsnoc . T.strip
isQuestion s =
  case (T.unsnoc . T.strip) s of
    Just (_, last) -> last == '?'
    Nothing -> False

-- isNonEmptyAllUpper = maybe False (T.all isUpper . uncurry T.cons) . T.uncons
isNonEmptyAllUpper s =
  case T.uncons s of
    Just (_, _) -> T.all isUpper s
    Nothing -> False

isShouty = liftA2 (||) isEmpty (isNonEmptyAllUpper . T.filter isLetter . T.strip)

isEmpty = liftA2 (||) T.null (T.all isSpace)