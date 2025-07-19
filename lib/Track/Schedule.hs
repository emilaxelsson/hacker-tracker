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

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty.Zipper (Zipper, fromNonEmpty)
import Oops (oops)
import Player.Config (PlayerConfig (..))
import Protolude
import Track.AST
    ( BPM (..)
    , InstrumentAcr
    , InstrumentTarget
    , NoteName (..)
    , Pitch (..)
    , Resolution (..)
    , SourceLine (..)
    , Velocity
    )
import Track.AST qualified as AST
import Track.Check (checkPatterns)

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

compileNote :: HashMap InstrumentAcr InstrumentTarget -> AST.Note -> Note
compileNote instrumentMap AST.Note{instrument, velocity, pitch} =
    Note
        { -- The parser has already checked that the track doesn't refer to undefined instruments
          -- TODO Move this check out of the parser
          instrument = fromMaybe (oops "should not happen") $ HM.lookup instrument instrumentMap
        , velocity = fromMaybe defaultVelocity velocity
        , pitch = fromMaybe defaultPitch pitch
        }

scheduleRow
    :: PlayerConfig
    -> BPM
    -> Resolution
    -> HashMap InstrumentAcr InstrumentTarget
    -> Beat
    -- ^ The beat on which the row starts
    -> AST.Row
    -> (Beat, Row)
    -- ^ The beat on which the next row starts
scheduleRow config bpm resolution instrumentMap beat AST.Row{rowSourceLine, notes} =
    (beat', scheduledRow)
  where
    beat' = beat + 1 / fromIntegral resolution
    scheduledRow =
        Row
            { at = beatToTick config bpm beat
            , rowSourceLine
            , notes = map (compileNote instrumentMap) notes
            }

schedulePattern
    :: PlayerConfig
    -> AST.TrackConfig
    -> HashMap InstrumentAcr InstrumentTarget
    -> Beat
    -- ^ The beat on which the pattern starts
    -> AST.Pattern NonEmpty
    -> (Beat, Pattern)
    -- ^ The beat on which the next pattern starts
schedulePattern playerConfig AST.TrackConfig{bpm} instrumentMap beat AST.Pattern{patternTitle, resolution, rows} =
    second mkPatternSchedule $
        mapAccumL (scheduleRow playerConfig bpm resolution instrumentMap) beat rows
  where
    mkPatternSchedule scheduledRows =
        Pattern
            { patternTitle
            , rows = scheduledRows
            }

-- | The resulting 'Tick' is the length of the track plus one tick
scheduleTrack
    :: PlayerConfig
    -> HashMap InstrumentAcr InstrumentTarget
    -> AST.Track
    -> Either Text Track
scheduleTrack playerConfig instrumentMap track = do
    trackPatterns <- checkPatterns track
    let (_, patterns) =
            mapAccumL
                (schedulePattern playerConfig (AST.config track) instrumentMap)
                startBeat
                trackPatterns
    return $ fromNonEmpty patterns
  where
    startBeat :: Beat
    startBeat = 0
