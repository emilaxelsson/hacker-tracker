module ParserSpec (spec) where

import Protolude
import Test.Hspec
import Track.Parser (parseTrack)

spec :: Spec
spec = do
    it "can parse test-track.md" $ do
        track <- readFile "tests/test-track.md"

        parseTrack track
            `shouldSatisfy` isRight
