module Time
    ( ElapsedTime (..)
    , prettyElapsedTime
    , secondsToElapsedTime
    ) where

import Data.Fixed (Fixed (..), Milli, divMod')
import Data.Text qualified as Text
import Protolude

data ElapsedTime = ElapsedTime
    { hours :: Word
    , minutes :: Word
    , seconds :: Word
    , milliseconds :: Word
    }
    deriving stock (Show)

-- |
-- >>> prettyElapsedTime ElapsedTime {hours = 1, minutes = 6, seconds = 40, milliseconds = 84}
-- "01:06:40.084"
prettyElapsedTime :: ElapsedTime -> Text
prettyElapsedTime ElapsedTime{hours, minutes, seconds, milliseconds} =
    mconcat
        [ pad0 2 $ show hours
        , ":"
        , pad0 2 $ show minutes
        , ":"
        , pad0 2 $ show seconds
        , "."
        , pad0 3 $ show milliseconds
        ]
  where
    pad0 n = Text.takeEnd n . ("00" <>)

-- |
-- >>> secondsToElapsedTime 4000.1041
-- ElapsedTime {hours = 1, minutes = 6, seconds = 40, milliseconds = 104}
secondsToElapsedTime :: Milli -> ElapsedTime
secondsToElapsedTime (MkFixed totalMillis) =
    ElapsedTime{hours, minutes, seconds, milliseconds = fromIntegral millis}
  where
    (hours, hourMillis) = totalMillis `divMod'` (60 * 60 * 1000)
    (minutes, minuteMillis) = hourMillis `divMod'` (60 * 1000)
    (seconds, millis) = minuteMillis `divMod'` 1000
