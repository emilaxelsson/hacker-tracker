{-# LANGUAGE Arrows #-}

module Main where

import Brick.BChan (BChan, newBChan, writeBChan)
import Data.Fixed (Fixed (MkFixed))
import Protolude
import Synch (SF (..), action, counter, everyN, runSF)
import Synch.System (execSystemForever)
import Synch.System qualified as System
import TUI (AppEvent (..), PlayerCommand (..), trackerMain)
import Time (prettyElapsedTime, secondsToElapsedTime)
import Track.Parser (parseTrack)

playerMillisPerTick :: Int
playerMillisPerTick = 15

reporter :: BChan AppEvent -> SF IO Int ()
reporter eventChan =
    action $ \ticks ->
        let elapsedMillis = ticks * playerMillisPerTick
            elapsedPretty =
                prettyElapsedTime $
                    secondsToElapsedTime $
                        MkFixed $
                            fromIntegral elapsedMillis
         in writeBChan eventChan $ NowPlaying elapsedPretty

player :: BChan AppEvent -> SF IO () ()
player eventChan = proc () -> do
    c <- counter -< True
    void (everyN 10 (reporter eventChan)) -< c

main :: IO ()
main = do
    track <- readFile "tests/test-track.md"
    print $ parseTrack track

    sysControl <- System.newControl
    eventChan <- newBChan 1

    bracket
        ( forkIO $
            execSystemForever playerMillisPerTick sysControl $
                runSF $
                    player eventChan
        )
        killThread
        ( const $
            void $
                trackerMain
                    eventChan
                    ( \case
                        Start -> System.start sysControl
                        Stop -> System.stop sysControl
                    )
        )

    putStrLn ("exiting" :: Text)
