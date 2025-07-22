module CheckSpec (spec) where

import CMark (PosInfo (..))
import Data.Text qualified as Text
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Property, arbitrary, shrink, (===))
import Test.QuickCheck qualified as Q
import Track.AST
import Track.Check (checkPatterns)
import Track.Parser
import Track.PrettyPrinter

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

        it "fails a track without sections" $ do
            let ast =
                    Track
                        { config = TrackConfig 0 []
                        , sections = []
                        }

            checkPatterns ast
                `shouldSatisfy` isLeft

        it "fails a track without patterns" $ do
            let ast =
                    Track
                        { config = TrackConfig 0 []
                        , sections =
                            [ Section "" []
                            , Section "" []
                            ]
                        }

            checkPatterns ast
                `shouldSatisfy` isLeft

        it "fails a track that only has empty patterns" $ do
            let ast =
                    Track
                        { config = TrackConfig 0 []
                        , sections =
                            [ Section
                                ""
                                [ Pattern 0 "" 0 []
                                , Pattern 0 "" 0 []
                                ]
                            ]
                        }

            checkPatterns ast
                `shouldSatisfy` isLeft

        it "fails a track that refers to an undefined instrument" $ do
            let ast =
                    Track
                        { config = TrackConfig 0 [("PI", 0), ("BA", 1)]
                        , sections =
                            [ Section
                                ""
                                [ Pattern
                                    { patternSourceLine = 0
                                    , patternTitle = ""
                                    , resolution = 0
                                    , rows =
                                        [ Row 0
                                            [ Note nullPos "PI" Nothing Nothing
                                            , Note nullPos "ZZ" Nothing Nothing
                                            , Note nullPos "BA" Nothing Nothing
                                            ]
                                        ]
                                    }
                                ]
                            ]
                        }

            checkPatterns ast
                `shouldSatisfy` isLeft
