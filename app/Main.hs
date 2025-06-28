module Main where

import Protolude
import Track.Parser (parseTrack)

main :: IO ()
main = do
    track <- readFile "examples/test.md"
    print $ parseTrack track
