{-# OPTIONS_GHC -Wno-orphans #-}

module ParserSpec (spec) where

import CMark (PosInfo (..))
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Property, arbitrary, (===))
import Test.QuickCheck qualified as Q
import Track.Parser
import Track.PrettyPrinter
import Track.Types

knownInstruments :: [InstrumentAcr]
knownInstruments = ["AA", "BB", "CC", "DD"]

nullPos :: PosInfo
nullPos = PosInfo 0 0 0 0

instance Arbitrary NoteName where
    arbitrary = Q.arbitraryBoundedEnum
    shrink n
        | n == minBound = []
        | otherwise = [pred n]

instance Arbitrary Pitch where
    arbitrary =
        Pitch
            <$> Q.arbitraryBoundedEnum
            <*> Q.liftArbitrary (Q.choose (0, 8))

    shrink = Q.genericShrink

instance Arbitrary InstrumentAcr where
    arbitrary = Q.oneof $ map return $ toList knownInstruments

instance Arbitrary Velocity where
    arbitrary = Velocity <$> Q.choose (0, 100)
    shrink = map Velocity . Q.shrink . unVelocity

instance Arbitrary Note where
    arbitrary = Note <$> arbitrary <*> arbitrary <*> arbitrary
    shrink = Q.genericShrink

prop_parseNote :: Note -> Property
prop_parseNote n =
    Q.property $
        let n' = parseNote knownInstruments nullPos $ prettyNote n
         in n' === Right n

spec :: Spec
spec = do
    describe "parseNote" $ do
        prop "roundtrip" prop_parseNote

    describe "parseTrack" $ do
        it "can parse test-track.md" $ do
            track <- readFile "tests/test-track.md"

            parseTrack track
                `shouldSatisfy` isRight
