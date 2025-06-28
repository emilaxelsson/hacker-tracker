module ParserSpec (spec) where

import Protolude
import Track.Parser (parseTrack)
import Test.Hspec

spec :: Spec
spec = do
    it "can parse test-track.md" $ do
        track <- readFile "tests/test-track.md"

        parseTrack track
            `shouldSatisfy` isRight
