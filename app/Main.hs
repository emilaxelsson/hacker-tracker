{-# LANGUAGE Arrows #-}

module Main where

import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Arrow (returnA)
import Data.Fixed (Fixed (MkFixed))
import Data.IORef (IORef, newIORef, writeIORef)
import Protolude
import Synch (SF (..), action, counter, everyN, refInput, runSF)
import Synch.System (execSystemForever)
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

player :: BChan AppEvent -> IORef Bool -> SF IO () ()
player eventChan runningRef = proc () -> do
    running <- refInput runningRef -< ()

    if running
        then do
            c <- counter -< True
            void (everyN 10 (reporter eventChan)) -< c
        else do
            returnA -< ()

main :: IO ()
main = do
    track <- readFile "tests/test-track.md"
    print $ parseTrack track

    eventChan <- newBChan 1
    runningRef <- newIORef False

    bracket
        ( forkIO $
            execSystemForever playerMillisPerTick $
                runSF $
                    player eventChan runningRef
        )
        killThread
        ( const $
            void $
                trackerMain
                    eventChan
                    ( \case
                        Start -> writeIORef runningRef True
                        Stop -> writeIORef runningRef False
                    )
        )

    putStrLn ("exiting" :: Text)
