module Track.Check
    ( checkPatterns
    ) where

import Protolude hiding (note)
import Track.AST (Note, Pattern (..), Section (..), Track (..))

nonEmptyPattern :: Pattern [] note -> Maybe (Pattern NonEmpty note)
nonEmptyPattern pat@Pattern{rows} =
    nonEmpty rows
        <&> \rs -> pat{rows = rs}

checkPatterns :: Track -> Either Text (NonEmpty (Pattern NonEmpty Note))
checkPatterns Track{sections} =
    maybe (Left "Track has no patterns.") Right $
        nonEmpty $
            concatMap (mapMaybe nonEmptyPattern . patterns) sections
