{-# LANGUAGE Arrows #-}

module Player
    ( player
    , playerMillisPerTick
    ) where

import Control.Arrow (Arrow (arr), (<<<), (>>>))
import Data.Fixed (Fixed (MkFixed))
import Protolude
import Synch (Event, SF (..), counter, everyN, latch)
import Synch.RefStore (RefStore)
import TUI (AppEvent (..))
import Time (prettyElapsedTime, secondsToElapsedTime)

playerMillisPerTick :: Int
playerMillisPerTick = 15

reporter :: Monad m => SF m (Maybe Int) (Event AppEvent)
reporter = arr $ fmap $ \ticks ->
    let elapsedMillis = ticks * playerMillisPerTick
        elapsedPretty =
            prettyElapsedTime $
                secondsToElapsedTime $
                    MkFixed $
                        fromIntegral elapsedMillis
     in NowPlaying elapsedPretty

player' :: RefStore m => SF m () Int
player' = arr (const True) >>> counter

pausablePlayer :: RefStore m => SF m Bool (Maybe Int)
pausablePlayer = proc running -> do
    ticks <-
        if running
            then arr (Just . Just) <<< player' -< ()
            else arr (const Nothing) -< ()
    latch Nothing -< ticks

player :: RefStore m => SF m Bool (Event AppEvent)
player = proc running -> do
    pos <- pausablePlayer -< running
    fmap join (everyN 10 reporter) -< pos
