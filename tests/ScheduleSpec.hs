module ScheduleSpec (spec) where

import Protolude
import Test.Hspec
import Track.AST (Track (config))
import Track.Check (checkPatterns)
import Track.Parser
import Track.Schedule

assertValidSchedule :: Foldable f => f Row -> Expectation
assertValidSchedule rows = do
    let timestamps = map at $ toList rows
    timestamps `shouldBe`
        sort timestamps

spec :: Spec
spec = do
    describe "scheduleTrack" $ do
        it "schedules test-track.md" $ do
            track <- readFile "tests/test-track.md"
            Right ast <- return $ parseTrack track
            Right checkedPatterns <- return $ checkPatterns ast

            let scheduledTrack =
                    scheduleTrack
                        PlayerConfig
                            { sourceFile = "tests/test-track.md"
                            , millisPerTick = 15
                            }
                        (config ast)
                        checkedPatterns

            length scheduledTrack
                `shouldBe` 2

            [p1, p2] <- return $ toList scheduledTrack

            let allRows = rows p1 <> rows p2

            length allRows
                `shouldBe` (16 + 4)

            assertValidSchedule allRows
