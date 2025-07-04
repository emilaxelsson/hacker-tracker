module Track.Types
    ( module Track.Types
    ) where

import Data.HashMap.Strict (HashMap)
import Protolude

-- | Acronym representing an instrument in the tracker source
newtype InstrumentAcr = InstrumentAcr {unInstrumentAcr :: Text}
    deriving newtype (Show, Eq, Hashable)

-- | Instrument number in the target output
newtype InstrumentTarget = InstrumentTarget {unInstrumentTarget :: Int}
    deriving newtype (Show)

newtype Velocity = Velocity {unVelocity :: Int}
    deriving newtype (Show)

data Pitch = Pitch
    { pitch :: Int
    , octave :: Maybe Int
    }
    deriving stock (Show)

data Note = Note
    { instrument :: InstrumentAcr
    , velocity :: Maybe Velocity
    , pitch :: Maybe Pitch
    }
    deriving stock (Show)

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
