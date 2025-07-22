module CheckSpec (spec) where

import CMark (PosInfo (..))
import Protolude
import Test.Hspec
import Track.AST
import Track.Check (checkPatterns)
import Track.Parser

nullPos :: PosInfo
nullPos = PosInfo 0 0 0 0

spec :: Spec
spec = do
    describe "checkPatterns" $ do
        it "accepts test-track.md" $ do
            track <- readFile "tests/test-track.md"
            Right ast <- return $ parseTrack track

            checkPatterns ast
                `shouldSatisfy` isRight

        let emptyConfig = TrackConfig 10 []

        it "fails a track without sections" $ do
            let ast =
                    Track
                        { config = emptyConfig
                        , sections = []
                        }

            let result = checkPatterns ast

            result `shouldSatisfy` isLeft
            show result `shouldContain` "no patterns"

        it "fails a track without patterns" $ do
            let ast =
                    Track
                        { config = emptyConfig
                        , sections =
                            [ Section "" []
                            , Section "" []
                            ]
                        }

            let result = checkPatterns ast

            result `shouldSatisfy` isLeft
            show result `shouldContain` "no patterns"

        it "fails a track that only has empty patterns" $ do
            let ast =
                    Track
                        { config = emptyConfig
                        , sections =
                            [ Section
                                ""
                                [ Pattern nullPos "" 0 []
                                , Pattern nullPos "" 0 []
                                ]
                            ]
                        }

            let result = checkPatterns ast

            result `shouldSatisfy` isLeft
            show result `shouldContain` "no patterns"

        it "fails a track that refers to an undefined instrument" $ do
            let ast =
                    Track
                        { config = TrackConfig 10 [("PI", 0), ("BA", 1)]
                        , sections =
                            [ Section
                                ""
                                [ Pattern
                                    nullPos
                                    ""
                                    10
                                    [ Row
                                        0
                                        [ Note nullPos "PI" Nothing Nothing
                                        , Note nullPos "ZZ" Nothing Nothing
                                        , Note nullPos "BA" Nothing Nothing
                                        ]
                                    ]
                                ]
                            ]
                        }

            let result = checkPatterns ast

            result `shouldSatisfy` isLeft
            show result `shouldContain` "ZZ"

        it "fails a track that has a non-positive BPM value" $ do
            let ast =
                    Track
                        { config = TrackConfig (-2) []
                        , sections =
                            [ Section "" [Pattern nullPos "" 10 [Row 0 []]]
                            ]
                        }

            let result = checkPatterns ast

            result `shouldSatisfy` isLeft
            show result `shouldContain` "BPM"

        it "fails a track that has patterns with a non-positive resolution value" $ do
            let ast =
                    Track
                        { config = emptyConfig
                        , sections =
                            [ Section "" [Pattern nullPos "" (-2) [Row 0 []]]
                            ]
                        }

            let result = checkPatterns ast

            result `shouldSatisfy` isLeft
            show result `shouldContain` "Resolution"
