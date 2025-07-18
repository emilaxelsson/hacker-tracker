module Track.PrettyPrinter
    ( prettyPitch
    , prettyNote
    ) where

import Data.Text qualified as Text
import Protolude
import Track.AST

-- |
-- >>> prettyPitch (Pitch Cs Nothing)
-- "C#"
--
-- >>> prettyPitch (Pitch G (Just 5))
-- "G5"
prettyPitch :: Pitch -> Text
prettyPitch Pitch{noteName, octave} = name <> foldMap show octave
  where
    name = case noteName of
        A -> "A"
        As -> "A#"
        B -> "B"
        C -> "C"
        Cs -> "C#"
        D -> "D"
        Ds -> "D#"
        E -> "E"
        F -> "F"
        Fs -> "F#"
        G -> "G"
        Gs -> "G#"

-- |
-- >>> prettyNote (Note "HH" Nothing Nothing)
-- "HH"
--
-- >>> prettyNote (Note "HH" (Just 50) (Just (Pitch Cs Nothing)))
-- "HH-50-C#"
prettyNote :: Note -> Text
prettyNote Note{instrument = InstrumentAcr instr, velocity, pitch} =
    Text.intercalate "-" $
        filter
            (not . Text.null)
            [ instr
            , foldMap (show . unVelocity) velocity
            , foldMap prettyPitch pitch
            ]
