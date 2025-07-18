{-# LANGUAGE UndecidableInstances #-}

module Track.AST where

import Protolude

-- | The position of a line in the source code, starting from 1
newtype SourceLine = SourceLine {unSourceLine :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | Acronym representing an instrument in the tracker source
newtype InstrumentAcr = InstrumentAcr {unInstrumentAcr :: Text}
    deriving newtype (Eq, Ord, Show, Hashable, IsString)

-- | Instrument number in the target output
newtype InstrumentTarget = InstrumentTarget {unInstrumentTarget :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype Velocity = Velocity {unVelocity :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | Number of pattern rows per beat
newtype Resolution = Resolution {unResolution :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype BPM = BPM {unBPM :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

data NoteName
    = C
    | Cs
    | D
    | Ds
    | E
    | F
    | Fs
    | G
    | Gs
    | A
    | As
    | B
    deriving stock (Eq, Show, Enum, Bounded)

data Pitch = Pitch
    { noteName :: NoteName
    , octave :: Maybe Int
    -- ^ Relative to the \"default octave\" for the target player. 'Nothing' means the
    -- same as 0.
    }
    deriving stock (Eq, Show, Generic)

data Note = Note
    { instrument :: InstrumentAcr
    , velocity :: Maybe Velocity
    , pitch :: Maybe Pitch
    }
    deriving stock (Eq, Show, Generic)

data Row = Row
    { rowSourceLine :: SourceLine
    , notes :: [Note]
    }
    deriving stock (Show)

data Pattern f = Pattern
    { patternSourceLine :: Int
    , patternTitle :: Text
    , resolution :: Resolution
    , rows :: f Row
    }

deriving stock instance Show (f Row) => Show (Pattern f)

data Section = Section
    { sectionTitle :: Text
    , patterns :: [Pattern []]
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
