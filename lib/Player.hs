{-# LANGUAGE Arrows #-}

module Player
    ( player
    , playerMillisPerTick
    ) where

import Control.Arrow (Arrow (arr), (>>>), (<<<))
import Data.Fixed (Fixed (MkFixed))
import Protolude
import Synch (Event, SF (..), action, counter, everyN, latch)
import TUI (AppEvent (..))
import Time (prettyElapsedTime, secondsToElapsedTime)

playerMillisPerTick :: Int
playerMillisPerTick = 15

reporter :: SF IO (Maybe Int) (Event AppEvent)
reporter =
    action $ \case
        Nothing -> return Nothing
        Just ticks ->
            let elapsedMillis = ticks * playerMillisPerTick
                elapsedPretty =
                    prettyElapsedTime $
                        secondsToElapsedTime $
                            MkFixed $
                                fromIntegral elapsedMillis
             in return $ Just $ NowPlaying elapsedPretty

player' :: SF IO () Int
player' = arr (const True) >>> counter

pausablePlayer :: SF IO Bool (Maybe Int)
pausablePlayer = proc running -> do
    ticks <- if running
        then (Just . Just <$> player') -< ()
        else arr (const Nothing) -< ()
    latch Nothing -< ticks

player :: SF IO (Event AppEvent) () -> SF IO () Bool -> SF IO () ()
player sinkAppEvent isRunning = proc () -> do
    pos <- pausablePlayer <<< isRunning -< ()
    void (everyN 10 (reporter >>> sinkAppEvent)) -< pos
