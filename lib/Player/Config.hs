module Player.Config
    ( PlayerConfig (..)
    ) where

import Protolude

data PlayerConfig = PlayerConfig
    { millisPerTick :: Int
    , sourceFile :: FilePath
    }
    deriving stock (Eq, Show)
