{-# LANGUAGE Arrows #-}

module Player
    ( player
    ) where

import Control.Arrow (Arrow (arr), (<<<), (>>>))
import Data.Fixed (Fixed (MkFixed))
import Player.Config (PlayerConfig (PlayerConfig, millisPerTick))
import Protolude
import Synch
    ( Event
    , SF (..)
    , counter
    , everyN
    , latch
    , onEvent
    , positiveEdge
    )
import Synch.RefStore (RefStore)
import TUI (AppEvent (..))
import Time (prettyElapsedTime, secondsToElapsedTime)

reporter :: Monad m => PlayerConfig -> SF m (Maybe Int) (Event AppEvent)
reporter PlayerConfig{millisPerTick} = arr $ fmap $ \ticks ->
    let elapsedMillis = ticks * millisPerTick
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

player :: RefStore m => PlayerConfig -> SF m Bool (Event AppEvent)
player config = proc running -> do
    pos <- pausablePlayer -< running
    slowTick <- everyN 10 -< ()
    justPaused <- positiveEdge -< not running

    -- We want to avoid updating the UI too often, so we only do it when there's a slow
    -- tick or when the player was just paused. The application feels "laggy" if the UI is
    -- updated some time after the pause. That's why we update immediately on pause.
    let updateUI = slowTick <|> justPaused
    arr join <<< onEvent (reporter config) -< fmap (const pos) updateUI
