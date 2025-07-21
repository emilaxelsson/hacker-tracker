{-# OPTIONS_GHC -Wno-orphans #-}

module ParserSpec (spec) where

import CMark (PosInfo (..))
import Data.Text qualified as Text
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Property, arbitrary, shrink, (===))
import Test.QuickCheck qualified as Q
import Track.AST
import Track.Parser
import Track.PrettyPrinter

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
    arbitrary = do
        n <- Q.choose (1, 5)
        fmap (InstrumentAcr . Text.pack) $ replicateM n $ Q.choose ('A', 'Z')

instance Arbitrary Velocity where
    arbitrary = Velocity <$> Q.choose (0, 100)
    shrink = map Velocity . Q.shrink . unVelocity

instance Arbitrary Note where
    arbitrary = Note nullPos <$> arbitrary <*> arbitrary <*> arbitrary
    shrink Note{noteSourcePos, instrument, velocity, pitch} =
        [Note noteSourcePos i v p | (i, v, p) <- shrink (instrument, velocity, pitch)]

prop_parseNote :: Note -> Property
prop_parseNote n =
    Q.property $
        let n' = parseNote nullPos $ prettyNote n
         in n' === Right n

spec :: Spec
spec = do
    describe "parseNote" $ do
        prop "roundtrip" prop_parseNote

        describe "with configured instruments" $ do
            it "parses known instruments" $ do
                parseNote nullPos "BB-50-C"
                    `shouldBe` Right (Note nullPos "BB" (Just 50) (Just $ Pitch C Nothing))

    describe "parseTrack" $ do
        it "can parse test-track.md" $ do
            track <- readFile "tests/test-track.md"

            parseTrack track
                `shouldSatisfy` isRight
