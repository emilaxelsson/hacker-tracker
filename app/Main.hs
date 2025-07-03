{-# LANGUAGE Arrows #-}

module Main where

import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Arrow (Arrow (arr), (>>>))
import Data.Fixed (Fixed (MkFixed))
import Data.IORef (IORef, newIORef, writeIORef)
import Protolude
import Synch (SF (..), action, counter, everyN, latch, refInput, runSF, whenA)
import Synch.System (execSystemForever)
import TUI (AppEvent (..), PlayerCommand (..), trackerMain)
import Time (prettyElapsedTime, secondsToElapsedTime)
import Track.Parser (parseTrack)

playerMillisPerTick :: Int
playerMillisPerTick = 15

reporter :: BChan AppEvent -> SF IO (Maybe Int) ()
reporter eventChan =
    action $ \case
        Nothing -> return ()
        Just ticks ->
            let elapsedMillis = ticks * playerMillisPerTick
                elapsedPretty =
                    prettyElapsedTime $
                        secondsToElapsedTime $
                            MkFixed $
                                fromIntegral elapsedMillis
             in writeBChan eventChan $ NowPlaying elapsedPretty

player' :: SF IO () Int
player' = arr (const True) >>> counter

pausablePlayer :: IORef Bool -> SF IO () (Maybe Int)
pausablePlayer runningRef =
    whenA
        (refInput runningRef)
        (Just <$> player')
        >>> latch Nothing

player :: BChan AppEvent -> IORef Bool -> SF IO () ()
player eventChan runningRef = proc () -> do
    pos <- pausablePlayer runningRef -< ()
    void (everyN 10 (reporter eventChan)) -< pos

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
