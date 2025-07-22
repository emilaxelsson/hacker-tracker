{-# LANGUAGE UndecidableInstances #-}

module Track.AST where

import CMark qualified as MD
import Protolude hiding (note)

-- | The position of a line in the source code, starting from 1
newtype SourceLine = SourceLine {unSourceLine :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | Acronym representing an instrument in the tracker source
newtype InstrumentAcr = InstrumentAcr {unInstrumentAcr :: Text}
    deriving newtype (Eq, Ord, Show, Hashable, IsString)

-- | Instrument number in the target output
newtype InstrumentTarget = InstrumentTarget {unInstrumentTarget :: Int}
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | A value in the range [0, 100]
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
    { noteSourcePos :: MD.PosInfo
    , instrument :: InstrumentAcr
    , velocity :: Maybe Velocity
    , pitch :: Maybe Pitch
    }
    deriving stock (Eq, Show, Generic)

data Row note = Row
    { rowSourceLine :: SourceLine
    , notes :: [note]
    }
    deriving stock (Show, Functor, Foldable, Traversable)

data Pattern f note = Pattern
    { patternSourcePos :: MD.PosInfo
    , patternTitle :: Text
    , resolution :: Resolution
    , rows :: f (Row note)
    }
    deriving stock (Functor, Foldable, Traversable)

deriving stock instance Show (f (Row note)) => Show (Pattern f note)

data Section = Section
    { sectionTitle :: Text
    , patterns :: [Pattern [] Note]
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
