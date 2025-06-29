module Main where

import Brick.BChan (newBChan, writeBChan)
import Data.IORef (newIORef, readIORef, writeIORef)
import Protolude
import TUI (AppEvent (..), PlayerCommand (..), trackerMain)
import Track.Parser (parseTrack)

main :: IO ()
main = do
    track <- readFile "tests/test-track.md"
    print $ parseTrack track

    playing <- newIORef False
    trackPosition <- newIORef (0 :: Int)

    eventChan <- newBChan 10

    let player = do
            threadDelay 500000
            isPlaying <- readIORef playing
            when isPlaying $ do
                pos <- readIORef trackPosition
                writeBChan eventChan $ NowPlaying $ "position: " <> show pos
                writeIORef trackPosition $ succ pos
            player

    void $ forkIO player

    void $
        trackerMain
            eventChan
            ( \case
                Start -> writeIORef playing True
                Stop -> writeIORef playing False
            )
