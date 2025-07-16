module Midi
    ( playNote
    ) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Vector.Storable qualified as Vector
import Oops
import Protolude
import Sound.RtMidi (OutputDevice, sendMessage)
import Track.Types
    ( InstrumentAcr
    , InstrumentTarget
    , Note (..)
    , NoteName (..)
    , Pitch (..)
    , Velocity
    )

defaultNote :: Pitch
defaultNote = Pitch C Nothing

defaultVelocity :: Velocity
defaultVelocity = 80

-- | Convert a 'Pitch' to a MIDI note number
--
-- The implementation is currently geared towards using Hydrogen as back end. Hydrogen
-- interprets MIDI note 36 (C2) as the default note (i.e. the sample without any
-- pitch alteration). Hence, we map \"C0\" to note 36.
midiNote :: Maybe Pitch -> Int
midiNote p =
    fromEnum noteName + octaveShift + 36
  where
    Pitch{noteName, octave} = fromMaybe defaultNote p
    octaveShift = fromMaybe 0 octave * 12

playNote :: HashMap InstrumentAcr InstrumentTarget -> OutputDevice -> Note -> IO ()
playNote instrumentMap device Note{instrument, velocity, pitch} = do
    let instr = fromMaybe (oops "TODO") $ HM.lookup instrument instrumentMap
    let instrumentWord = fromIntegral instr

    -- Set instrument
    -- TODO Keep track of last change, to avoid changing to the same instrument
    sendMessage device (Vector.fromList [0xc0, 89, instrumentWord])

    let vel = fromMaybe defaultVelocity velocity
    let velocityWord = round (fromIntegral vel / 100 * 127 :: Double)
    let noteWord = fromIntegral $ midiNote pitch

    -- Play note
    sendMessage device (Vector.fromList [0x90, noteWord, velocityWord])
