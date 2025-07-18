module Player.Schedule
    ( PlayerConfig (..)
    , Tick (..)
    , Note (..)
    , ScheduledRow (..)
    , PatternSchedule (..)
    , scheduleTrack
    ) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty.Zipper (Zipper, fromNonEmpty)
import Oops (oops)
import Player.Config (PlayerConfig (..))
import Protolude
import Track.Types
    ( BPM (..)
    , InstrumentAcr
    , InstrumentTarget
    , NoteName (..)
    , Pitch (..)
    , Resolution (..)
    , SourceLine (..)
    , Track (..)
    , Velocity
    )
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

data Note = Note
    { instrument :: InstrumentTarget
    , velocity :: Velocity
    , pitch :: Pitch
    }
    deriving stock (Eq, Show)

data ScheduledRow = ScheduledRow
    { at :: Tick
    , rowSourceLine :: SourceLine
    , notes :: [Note]
    }
    deriving stock (Eq, Show)

data PatternSchedule = PatternSchedule
    { patternTitle :: Text
    , rows :: NonEmpty ScheduledRow
    }
    deriving stock (Eq, Show)

-- | Find the nearest tick for the given beat
--
-- Ticks and beats are counted from the same reference point
beatToTick :: PlayerConfig -> BPM -> Beat -> Tick
beatToTick PlayerConfig{millisPerTick} bpm beat =
    round $ millis / fromIntegral millisPerTick
  where
    millisPerBeat = 60_000 / fromIntegral bpm
    millis = beat * fromRational millisPerBeat

interpretNote :: HashMap InstrumentAcr InstrumentTarget -> Track.Note -> Note
interpretNote instrumentMap Track.Note{instrument, velocity, pitch} =
    Note
        { instrument = fromMaybe (oops "should not happen") $ HM.lookup instrument instrumentMap
        , velocity = fromMaybe defaultVelocity velocity
        , pitch = fromMaybe defaultPitch pitch
        }
  where
    defaultVelocity :: Velocity
    defaultVelocity = 80
    defaultPitch = Pitch C Nothing

scheduleRow
    :: PlayerConfig
    -> BPM
    -> Resolution
    -> HashMap InstrumentAcr InstrumentTarget
    -> Beat
    -- ^ The beat on which the row starts
    -> Track.Row
    -> (Beat, ScheduledRow)
    -- ^ The beat on which the next row starts
scheduleRow config bpm resolution instrumentMap beat Track.Row{rowSourceLine, notes} =
    (beat', scheduledRow)
  where
    beat' = beat + 1 / fromIntegral resolution
    scheduledRow =
        ScheduledRow
            { at = beatToTick config bpm beat
            , rowSourceLine
            , notes = map (interpretNote instrumentMap) notes
            }

nonEmptyPattern :: Track.Pattern [] -> Maybe (Track.Pattern NonEmpty)
nonEmptyPattern pat@Track.Pattern{rows} =
    nonEmpty rows
        <&> \rs -> pat{Track.rows = rs}

schedulePattern
    :: PlayerConfig
    -> Track.TrackConfig
    -> HashMap InstrumentAcr InstrumentTarget
    -> Beat
    -- ^ The beat on which the pattern starts
    -> Track.Pattern NonEmpty
    -> (Beat, PatternSchedule)
    -- ^ The beat on which the next pattern starts
schedulePattern playerConfig Track.TrackConfig{bpm} instrumentMap beat Track.Pattern{patternTitle, resolution, rows} =
    second mkPatternSchedule $
        mapAccumL (scheduleRow playerConfig bpm resolution instrumentMap) beat rows
  where
    mkPatternSchedule scheduledRows =
        PatternSchedule
            { patternTitle
            , rows = scheduledRows
            }

-- | The resulting 'Tick' is the length of the track plus one tick
scheduleTrack
    :: PlayerConfig
    -> HashMap InstrumentAcr InstrumentTarget
    -> Track
    -> Either Text (Zipper PatternSchedule)
scheduleTrack playerConfig instrumentMap Track{config = trackConfig, sections} = do
    trackPatterns <-
        maybe (Left "Track has no patterns.") Right $
            nonEmpty $
                concatMap (mapMaybe nonEmptyPattern . Track.patterns) sections
    let (_, patterns) =
            mapAccumL (schedulePattern playerConfig trackConfig instrumentMap) startBeat $
                trackPatterns
    return $ fromNonEmpty patterns
  where
    startBeat :: Beat
    startBeat = 0
