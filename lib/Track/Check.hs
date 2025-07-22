module Track.Check
    ( CheckedNote (..)
    , CheckedPatterns
    , checkPatterns
    ) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Protolude hiding (note)
import Track.AST
    ( InstrumentAcr (unInstrumentAcr)
    , InstrumentTarget
    , Note (..)
    , Pattern (..)
    , Pitch
    , Section (..)
    , Track (..)
    , TrackConfig (..)
    , Velocity
    )
import Track.Parser (LocatedError)

data CheckedNote = CheckedNote
    { instrument :: InstrumentTarget
    , velocity :: Maybe Velocity
    , pitch :: Maybe Pitch
    }
    deriving stock (Eq, Show, Generic)

type CheckedPatterns = NonEmpty (Pattern NonEmpty CheckedNote)

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
    :: Track
    -> Either LocatedError CheckedPatterns
checkPatterns Track{config = TrackConfig{instruments}, sections} = do
    let instrAcrs = map fst instruments
    let dups = List.nub (instrAcrs List.\\ List.nub instrAcrs)
    unless (null dups) $
        Left $
            (Nothing,) $
                "Multiple definitions of instruments: " <> show dups

    let instrumentMap = HM.fromList instruments

    nePatterns <-
        maybe (Left (Nothing, "Track has no patterns.")) Right $
            nonEmpty $
                concatMap (mapMaybe nonEmptyPattern . patterns) sections

    mapM (mapM (checkNote instrumentMap)) nePatterns
