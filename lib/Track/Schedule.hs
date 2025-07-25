-- | A schedule can be seen as a compiled track
module Track.Schedule
    ( PlayerConfig (..)
    , Tick (..)
    , Note (..)
    , Row (..)
    , Pattern (..)
    , Track
    , scheduleTrack
    ) where

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Zipper (Zipper, fromNonEmpty)
import Oops (oops)
import Player.Config (PlayerConfig (..))
import Protolude
import Track.AST
    ( BPM (..)
    , InstrumentTarget
    , NoteName (..)
    , Pitch (..)
    , Resolution (..)
    , SourceLine (..)
    , TrackConfig (..)
    , Velocity
    )
import Track.AST qualified as AST
import Track.Check (CheckedNote (..), CheckedPatterns)

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

data Row = Row
    { at :: Tick
    , rowSourceLine :: SourceLine
    , notes :: [Note]
    }
    deriving stock (Eq, Show)

instance Semigroup Row where
    Row _ _ notes1 <> Row at2 line2 notes2 =
        Row at2 line2 (notes1 <> notes2)

data Pattern = Pattern
    { patternTitle :: Text
    , rows :: NonEmpty Row
    }
    deriving stock (Eq, Show)

type Track = Zipper Pattern

-- | Find the nearest tick for the given beat
--
-- Ticks and beats are interpreted as relative to a common reference point.
beatToTick :: PlayerConfig -> BPM -> Beat -> Tick
beatToTick PlayerConfig{millisPerTick} bpm beat =
    round $ millis / fromIntegral millisPerTick
  where
    millisPerBeat = 60_000 / fromIntegral bpm
    millis = beat * fromRational millisPerBeat

defaultVelocity :: Velocity
defaultVelocity = 80

defaultPitch :: Pitch
defaultPitch = Pitch C Nothing

interpretNote :: CheckedNote -> Note
interpretNote CheckedNote{instrument, velocity, pitch} =
    Note
        { instrument
        , velocity = fromMaybe defaultVelocity velocity
        , pitch = fromMaybe defaultPitch pitch
        }

scheduleRow
    :: PlayerConfig
    -> BPM
    -> Resolution
    -> Beat
    -- ^ The beat on which the row starts
    -> AST.Row CheckedNote
    -> (Beat, Row)
    -- ^ The beat on which the next row starts
scheduleRow config bpm resolution beat AST.Row{rowSourceLine, notes} =
    (beat', scheduledRow)
  where
    beat' = beat + 1 / fromIntegral resolution
    scheduledRow =
        Row
            { at = beatToTick config bpm beat
            , rowSourceLine
            , notes = map interpretNote notes
            }

schedulePattern
    :: PlayerConfig
    -> AST.TrackConfig
    -> Beat
    -- ^ The beat on which the pattern starts
    -> AST.Pattern NonEmpty CheckedNote
    -> (Beat, Pattern)
    -- ^ The beat on which the next pattern starts
schedulePattern playerConfig AST.TrackConfig{bpm} beat AST.Pattern{patternTitle, resolution, rows} =
    let (beat', pattern@Pattern{rows = rows'}) =
            second mkPatternSchedule $
                mapAccumL (scheduleRow playerConfig bpm resolution) beat rows

        lastRow = NE.last rows'

        -- Extra row scheduled at the last tick of the pattern, to ensure that the pattern
        -- lasts for its whole duration
        durationRow =
            Row
                { at = beatToTick playerConfig bpm beat' - 1
                , rowSourceLine = rowSourceLine lastRow
                , notes = []
                }
     in (beat', pattern{rows = rows' <> pure durationRow})
  where
    mkPatternSchedule scheduledRows =
        Pattern
            { patternTitle
            , rows = scheduledRows
            }

-- | The resulting 'Tick' is the length of the track plus one tick
scheduleTrack :: PlayerConfig -> TrackConfig -> CheckedPatterns -> Track
scheduleTrack playerConfig@PlayerConfig{millisPerTick} trackConfig patterns
    | millisPerTick <= 0 =
        oops $
            "millisPerTick must be a positive integer, got: " <> show millisPerTick
    | otherwise =
        fromNonEmpty $
            snd $
                mapAccumL
                    (schedulePattern playerConfig trackConfig)
                    startBeat
                    patterns
  where
    startBeat :: Beat
    startBeat = 0
