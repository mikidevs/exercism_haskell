module Bob (responseFor) where
import Data.Char (isAscii, isLetter, isUpper, isSpace)

responseFor :: String -> String
responseFor xs
  | shoutyQuestion = "Calm down, I know what I'm doing!"
  | question = "Sure."
  | shout = "Whoa, chill out!"
  | silence = "Fine. Be that way!"
  | otherwise = "Whatever."
  where
    question = isQuestion xs
    shout = isShouty xs 
    shoutyQuestion = isShouty xs && isQuestion xs
    silence = isEmpty xs

isQuestion s = 
  let letters = stripKeepQ s
  in not (isEmpty letters) && last letters == '?'

isQuestionMarkOrUpper = liftA2 (||) (== '?') isUpper

isShouty s = 
  let letters = strip s
  in not (isEmpty letters) && all isQuestionMarkOrUpper letters

isEmpty s = s == "" || all isSpace s

stripKeepQ s = [c | c <- s, isAscii c && (c == '?' || isLetter c)]

strip s = [c | c <- s, isAscii c && isLetter c]