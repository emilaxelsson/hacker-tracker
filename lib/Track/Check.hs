module Track.Check
    ( CheckedNote (..)
    , checkPatterns
    ) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Protolude hiding (note)
import Track.AST
    ( InstrumentAcr (unInstrumentAcr)
    , InstrumentTarget
    , Note (..)
    , Pattern (..)
    , Pitch
    , Section (..)
    , Track (..)
    , Velocity
    )
import Track.Parser (LocatedError)

data CheckedNote = CheckedNote
    { instrument :: InstrumentTarget
    , velocity :: Maybe Velocity
    , pitch :: Maybe Pitch
    }
    deriving stock (Eq, Show, Generic)

nonEmptyPattern :: Pattern [] note -> Maybe (Pattern NonEmpty note)
nonEmptyPattern pat@Pattern{rows} =
    nonEmpty rows
        <&> \rs -> pat{rows = rs}

checkNote
    :: HashMap InstrumentAcr InstrumentTarget
    -> Note
    -> Either LocatedError CheckedNote
checkNote instrumentMap Note{noteSourcePos, instrument = acr, velocity, pitch} = do
    target <-
        maybe
            (Left (Just noteSourcePos, "Undefined instrument '" <> unInstrumentAcr acr <> "'."))
            return
            $ HM.lookup acr instrumentMap
    return $
        CheckedNote
            { instrument = target
            , velocity
            , pitch
            }

checkPatterns
    :: HashMap InstrumentAcr InstrumentTarget
    -> Track
    -> Either LocatedError (NonEmpty (Pattern NonEmpty CheckedNote))
checkPatterns instrumentMap Track{sections} = do
    nePatterns <-
        maybe (Left (Nothing, "Track has no patterns.")) Right $
            nonEmpty $
                concatMap (mapMaybe nonEmptyPattern . patterns) sections
    mapM (mapM (checkNote instrumentMap)) nePatterns
