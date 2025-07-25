module ScheduleSpec (spec) where

import Data.List qualified as List
import Protolude
import Test.Hspec
import Track.AST (Track (..), TrackConfig (..))
import Track.Check (checkPatterns)
import Track.Parser
import Track.Schedule

isSorted :: Ord a => [a] -> Bool
isSorted as = as == sort as

assertValidSchedule :: Foldable f => f Row -> Expectation
assertValidSchedule rows = do
    let timestamps = map at $ toList rows
    timestamps
        `shouldSatisfy` isSorted

spec :: Spec
spec = do
    describe "scheduleTrack" $ do
        it "schedules test-track.md" $ do
            track <- readFile "tests/test-track.md"
            Right ast <- return $ parseTrack track
            Right checkedPatterns <- return $ checkPatterns ast

            let millisPerTick = 15 :: Int

            let scheduledTrack =
                    scheduleTrack
                        PlayerConfig
                            { sourceFile = "tests/test-track.md"
                            , millisPerTick
                            }
                        (config ast)
                        checkedPatterns

            length scheduledTrack
                `shouldBe` 2

            [p1, p2] <- return $ toList scheduledTrack

            let allRows = rows p1 <> rows p2

            let rowsPattern1 = 16 + 1 :: Int -- Duration row inserted at the end
            let rowsPattern2 = 4 + 1 :: Int

            length allRows
                `shouldBe` (rowsPattern1 + rowsPattern2)

            assertValidSchedule allRows

            let expectedLastTick =
                    let numBeats = 8 :: Rational
                        bpm' = fromIntegral (bpm (config ast)) :: Rational
                        minutes = numBeats / bpm'
                        millis = minutes * 60 * 1000
                     in millis / fromIntegral millisPerTick - 1
            let actualLastTick = List.last $ map at $ toList allRows

            actualLastTick
                `shouldBe` round expectedLastTick
