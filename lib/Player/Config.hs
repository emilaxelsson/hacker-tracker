module Player.Config
    ( PlayerConfig (..)
    ) where

import Protolude

data PlayerConfig = PlayerConfig
    { millisPerTick :: Int
    }
    deriving stock (Eq, Show)
