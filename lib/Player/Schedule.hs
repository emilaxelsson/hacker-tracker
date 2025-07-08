module Player.Schedule
    ( PlayerConfig (..)
    , Tick (..)
    , ScheduledRow (..)
    , PatternSchedule (..)
    , TrackSchedule
    , scheduleTrack
    ) where

import Data.List.Zipper (Zipper, fromList)
import Player.Config (PlayerConfig (..))
import Protolude
import Track.Types (BPM (..), Note (..), Resolution (..), Track (..))
import Track.Types qualified as Track

-- | Beat count from some reference point in the track (e.g. the start of the track)
--
-- Fractional beats are used to position e.g. eighth or sixteenth notes. E.g. 50.5 is the
-- eighth note between beat 50 and 51.
newtype Beat = Beat {unBeat :: Rational}
    deriving newtype (Eq, Ord, Show, Num, Fractional, RealFrac)

instance Real Beat where
    toRational = unBeat

-- | Tick count from some reference point in the track (e.g. the start of the track)
--
-- Tick resolution is determined by 'millisPerTick'.
newtype Tick = Tick {unTick :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

data ScheduledRow = ScheduledRow
    { at :: Tick
    , rowSourceLine :: Int
    , notes :: [Note]
    }
    deriving stock (Eq, Show)

data PatternSchedule = PatternSchedule
    { patternTitle :: Text
    , notes :: [ScheduledRow]
    }
    deriving stock (Eq, Show)

type TrackSchedule = Zipper PatternSchedule

-- | Find the nearest tick for the given beat
--
-- Ticks and beats are counted from the same reference point
beatToTick :: PlayerConfig -> BPM -> Beat -> Tick
beatToTick PlayerConfig{millisPerTick} bpm beat =
    round $ millis / fromIntegral millisPerTick
  where
    millisPerBeat = 60_000 / fromIntegral bpm
    millis = beat * fromRational millisPerBeat

scheduleRow
    :: PlayerConfig
    -> BPM
    -> Resolution
    -> Beat
    -- ^ The beat on which the row starts
    -> Track.Row
    -> (Beat, ScheduledRow)
    -- ^ The beat on which the next row starts
scheduleRow config bpm resolution beat Track.Row{rowSourceLine, notes} =
    (beat', scheduledRow)
  where
    beat' = beat + 1 / fromIntegral resolution
    at = beatToTick config bpm beat
    scheduledRow = ScheduledRow{at, rowSourceLine, notes}

schedulePattern
    :: PlayerConfig
    -> Track.TrackConfig
    -> Beat
    -- ^ The beat on which the pattern starts
    -> Track.Pattern
    -> (Beat, PatternSchedule)
    -- ^ The beat on which the next pattern starts
schedulePattern playerConfig Track.TrackConfig{bpm} beat Track.Pattern{patternTitle, resolution, rows} =
    second mkPatternSchedule $ mapAccumL (scheduleRow playerConfig bpm resolution) beat rows
  where
    mkPatternSchedule scheduledRows =
        PatternSchedule
            { patternTitle
            , notes = scheduledRows
            }

-- | The resulting 'Tick' is the length of the track plus one tick
scheduleTrack :: PlayerConfig -> Track -> (Beat, TrackSchedule)
scheduleTrack playerConfig Track{config = trackConfig, sections} =
    second fromList $
        mapAccumL (schedulePattern playerConfig trackConfig) startBeat $
            concatMap Track.patterns sections
  where
    startBeat :: Beat
    startBeat = 0
