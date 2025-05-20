module Clock (addDelta, fromHourMin ) where
import Text.Printf ( printf )

-- data Clock = Clock Int Int
--   deriving Eq

data Clock = Clock Int Int

instance Eq Clock where
  (==) :: Clock -> Clock -> Bool
  Clock h1 m1 == Clock h2 m2 = h1 == h2 && m1 == m2

instance Show Clock where
  show :: Clock -> String
  show (Clock h m) = printf "%02d" h ++ ":" ++ printf "%02d" m

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min =
  let (hours, mins) =
        if min >= 60 || min < 0 then (min `div` 60, min `mod` 60) else (0, min)
  in
    Clock ((hours + hour) `mod` 24) mins

toString :: Clock -> String
toString (Clock h m) = printf "%02d" h ++ ":" ++ printf "%02d" m

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) =
  fromHourMin (hour + h) (min + m)