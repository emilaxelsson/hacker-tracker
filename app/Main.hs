module Main where

import Parser (parseSong)
import Protolude

main :: IO ()
main = do
    song <- readFile "seq.md"
    print $ parseSong song
