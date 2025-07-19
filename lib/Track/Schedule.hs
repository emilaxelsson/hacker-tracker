module Track.Schedule
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
import Track.AST
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
import Track.AST qualified as AST

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

interpretNote :: HashMap InstrumentAcr InstrumentTarget -> AST.Note -> Note
interpretNote instrumentMap AST.Note{instrument, velocity, pitch} =
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
    -> AST.Row
    -> (Beat, ScheduledRow)
    -- ^ The beat on which the next row starts
scheduleRow config bpm resolution instrumentMap beat AST.Row{rowSourceLine, notes} =
    (beat', scheduledRow)
  where
    beat' = beat + 1 / fromIntegral resolution
    scheduledRow =
        ScheduledRow
            { at = beatToTick config bpm beat
            , rowSourceLine
            , notes = map (interpretNote instrumentMap) notes
            }

nonEmptyPattern :: AST.Pattern [] -> Maybe (AST.Pattern NonEmpty)
nonEmptyPattern pat@AST.Pattern{rows} =
    nonEmpty rows
        <&> \rs -> pat{AST.rows = rs}

schedulePattern
    :: PlayerConfig
    -> AST.TrackConfig
    -> HashMap InstrumentAcr InstrumentTarget
    -> Beat
    -- ^ The beat on which the pattern starts
    -> AST.Pattern NonEmpty
    -> (Beat, PatternSchedule)
    -- ^ The beat on which the next pattern starts
schedulePattern playerConfig AST.TrackConfig{bpm} instrumentMap beat AST.Pattern{patternTitle, resolution, rows} =
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
                concatMap (mapMaybe nonEmptyPattern . AST.patterns) sections
    let (_, patterns) =
            mapAccumL (schedulePattern playerConfig trackConfig instrumentMap) startBeat $
                trackPatterns
    return $ fromNonEmpty patterns
  where
    startBeat :: Beat
    startBeat = 0
