module Types
    ( module Types
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

newtype Pitch = Pitch {unPitch :: Int}
    deriving newtype (Show)

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

data SongConfig = SongConfig
    { bpm :: Int
    , instruments :: HashMap InstrumentAcr InstrumentTarget
    }
    deriving stock (Show)

data Song = Song
    { config :: SongConfig
    , sections :: [Section]
    }
    deriving stock (Show)
