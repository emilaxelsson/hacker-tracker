module Track.Check
    ( checkPatterns
    ) where

import Protolude
import Track.AST (Pattern (..), Section (..), Track (..))

nonEmptyPattern :: Pattern [] -> Maybe (Pattern NonEmpty)
nonEmptyPattern pat@Pattern{rows} =
    nonEmpty rows
        <&> \rs -> pat{rows = rs}

checkPatterns :: Track -> Either Text (NonEmpty (Pattern NonEmpty))
checkPatterns Track{sections} =
    maybe (Left "Track has no patterns.") Right $
        nonEmpty $
            concatMap (mapMaybe nonEmptyPattern . patterns) sections
