module Track.Types
    ( module Track.Types
    ) where

import Data.HashMap.Strict (HashMap)
import Protolude

-- | Acronym representing an instrument in the tracker source
newtype InstrumentAcr = InstrumentAcr {unInstrumentAcr :: Text}
    deriving newtype (Eq, Show, Hashable, IsString)

-- | Instrument number in the target output
newtype InstrumentTarget = InstrumentTarget {unInstrumentTarget :: Int}
    deriving newtype (Show)

newtype Velocity = Velocity {unVelocity :: Int}
    deriving newtype (Eq, Show, Num)

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
    , resolution :: Int
    -- ^ number of rows per beat
    , rows :: [Row]
    }
    deriving stock (Show)

data Section = Section
    { sectionTitle :: Text
    , patterns :: [Pattern]
    }
    deriving stock (Show)

data TrackConfig = TrackConfig
    { bpm :: Int
    , instruments :: HashMap InstrumentAcr InstrumentTarget
    }
    deriving stock (Show)

data Track = Track
    { config :: TrackConfig
    , sections :: [Section]
    }
    deriving stock (Show)
