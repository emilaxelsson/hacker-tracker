{-# LANGUAGE Arrows #-}

module Player
    ( player
    ) where

import Control.Arrow (Arrow (arr), (<<<))
import Control.Monad.Fix (MonadFix)
import Data.Fixed (Fixed (MkFixed))
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Text qualified as Text
import Player.Config (PlayerConfig (..))
import Protolude
import Synch
    ( Event
    , SF (..)
    , delay
    , everyN
    , latch
    , onEvent
    , positiveEdge
    )
import Synch.RefStore (RefStore)
import TUI (AppEvent (..))
import Time (prettyElapsedTime, secondsToElapsedTime)
import Track.AST (SourceLine)
import Track.Schedule (Note (..), Pattern (..), Row (..), Tick, Track)

data PlayerState = PlayerState
    { tick :: Tick
    , currentPattern :: NonEmpty Row
    , track :: Track
    }

step :: PlayerState -> (Event Row, PlayerState)
step PlayerState{tick, currentPattern, track} =
    -- \| at > tick = (Nothing, s{tick = tick + 1})
    -- \| otherwise =
    ( evCombinedRow
    , PlayerState{tick = tick', currentPattern = currentPattern', track = track'}
    )
  where
    rowsToPlay = NE.takeWhile ((tick >=) . at) currentPattern
    evCombinedRow = sconcat <$> nonEmpty rowsToPlay
    remainingRows = NE.dropWhile ((tick >=) . at) currentPattern

    (tick', track') = case remainingRows of
        [] -> maybe (0, Z.start track) (tick + 1,) $ Z.right track
        _ -> (tick + 1, track)

    currentPattern' = fromMaybe (rows $ Z.current track') $ nonEmpty remainingRows

-- (tick', track') = case currentPattern of
--     _ :| [] -> maybe (0, Z.start track) (tick + 1,) $ Z.right track
--     _ -> (tick + 1, track)

-- currentPattern' = case currentPattern of
--     _ :| [] -> rows $ Z.current track'
--     _ :| r : rs -> r :| rs

playerCore
    :: (RefStore m, MonadFix m) => PlayerState -> SF m () (Tick, Event Row)
playerCore initialState = proc () -> do
    rec let (row, s@PlayerState{tick}) = step s'
        s' <- delay initialState -< s
    arr identity -< (tick, row)

pausablePlayer
    :: (RefStore m, MonadFix m)
    => PlayerState
    -> SF m Bool (Tick, SourceLine, Event Row)
pausablePlayer initialState = proc running -> do
    coreEv <-
        if running
            then arr Just <<< playerCore initialState -< ()
            else arr (const Nothing) -< ()

    let tickEv = fst <$> coreEv
    let rowEv = snd =<< coreEv

    tick <- latch 0 -< tickEv
    sourceLine <- latch 1 -< (rowSourceLine <$> rowEv)
    arr identity -< (tick, sourceLine, rowEv)

reporter :: PlayerConfig -> Tick -> SourceLine -> AppEvent
reporter PlayerConfig{millisPerTick, sourceFile} ticks sourceLine =
    NowPlaying $ elapsedPretty <> "  " <> Text.pack sourceFile <> ":" <> show sourceLine
  where
    elapsedMillis = fromIntegral ticks * millisPerTick
    elapsedSeconds = secondsToElapsedTime $ MkFixed $ fromIntegral elapsedMillis
    elapsedPretty = prettyElapsedTime elapsedSeconds

player
    :: (MonadFix m, RefStore m)
    => PlayerConfig
    -> Track
    -> SF m Bool (Event AppEvent, Event [Note])
player config track = proc running -> do
    (ticks, sourceLine, evRow) <- pausablePlayer initialState -< running
    slowTick <- everyN 10 -< ()
    justPaused <- positiveEdge -< not running

    -- We want to avoid updating the UI too often, so we only do it when there's a slow
    -- tick or when the player was just paused. The application feels "laggy" if the UI is
    -- updated some time after the pause. That's why we update immediately on pause.
    let updateUI = slowTick <|> justPaused
    appEvent <-
        onEvent (arr $ uncurry $ reporter config) -< fmap (const (ticks, sourceLine)) updateUI
    arr identity -< (appEvent, notes <$> evRow)
  where
    initialState =
        PlayerState
            { tick = 0
            , currentPattern = rows $ Z.current track
            , track
            }
