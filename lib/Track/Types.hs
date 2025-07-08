module Track.Types
    ( module Track.Types
    ) where

import Protolude

-- | Acronym representing an instrument in the tracker source
newtype InstrumentAcr = InstrumentAcr {unInstrumentAcr :: Text}
    deriving newtype (Eq, Ord, Show, Hashable, IsString)

-- | Instrument number in the target output
newtype InstrumentTarget = InstrumentTarget {unInstrumentTarget :: Int}
    deriving newtype (Show)

newtype Velocity = Velocity {unVelocity :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | Number of pattern rows per beat
newtype Resolution = Resolution {unResolution :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype BPM = BPM {unBPM :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

data NoteName
    = A
    | As
    | B
    | C
    | Cs
    | D
    | Ds
    | E
    | F
    | Fs
    | G
    | Gs
    deriving stock (Eq, Show, Enum, Bounded)

data Pitch = Pitch
    { noteName :: NoteName
    , octave :: Maybe Int
    }
    deriving stock (Eq, Show, Generic)

data Note = Note
    { instrument :: InstrumentAcr
    , velocity :: Maybe Velocity
    , pitch :: Maybe Pitch
    }
    deriving stock (Eq, Show, Generic)

data Row = Row
    { rowSourceLine :: Int
    , notes :: [Note]
    }
    deriving stock (Show)

data Pattern = Pattern
    { patternSourceLine :: Int
    , patternTitle :: Text
    , resolution :: Resolution
    , rows :: [Row]
    }
    deriving stock (Show)

data Section = Section
    { sectionTitle :: Text
    , patterns :: [Pattern]
    }
    deriving stock (Show)

data TrackConfig = TrackConfig
    { bpm :: BPM
    , instruments :: [(InstrumentAcr, InstrumentTarget)]
    }
    deriving stock (Show)

data Track = Track
    { config :: TrackConfig
    , sections :: [Section]
    }
    deriving stock (Show)
