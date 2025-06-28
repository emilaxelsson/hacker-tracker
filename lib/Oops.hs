module Oops (oops) where

import Protolude (HasCallStack)
import Prelude

oops :: HasCallStack => String -> a
oops msg = Prelude.error $ "Oops: " ++ msg
