module Midi
    ( playNote
    ) where

import Data.Vector.Storable qualified as Vector
import Player.Schedule (Note (..))
import Protolude
import Sound.RtMidi (OutputDevice, sendMessage)
import Track.Types (Pitch (..))

-- | Convert a 'Pitch' to a MIDI note number
--
-- The implementation is currently geared towards using Hydrogen as back end. Hydrogen
-- interprets MIDI note 36 (C2) as the default note (i.e. the sample without any
-- pitch alteration). Hence, we map \"C0\" to note 36.
midiNote :: Pitch -> Int
midiNote Pitch{noteName, octave} = fromEnum noteName + octaveShift + 36
  where
    octaveShift = fromMaybe 0 octave * 12

playNote :: OutputDevice -> Note -> IO ()
playNote device Note{instrument, velocity, pitch} = do
    let instrumentWord = fromIntegral instrument
    let velocityWord = round (fromIntegral velocity / 100 * 127 :: Double)
    let noteWord = fromIntegral $ midiNote pitch

    -- Set instrument
    -- TODO Keep track of last change, to avoid changing to the same instrument
    sendMessage device (Vector.fromList [0xc0, 89, instrumentWord])

    -- Play note
    sendMessage device (Vector.fromList [0x90, noteWord, velocityWord])
