module Main where

import Brick.BChan (newBChan, writeBChan)
import Control.Arrow ((>>>))
import Data.IORef (newIORef, writeIORef)
import Player (player, playerMillisPerTick)
import Protolude
import Synch (action, onEvent, refInput, runSF)
import Synch.System (execSystemForever)
import TUI (PlayerCommand (..), trackerMain)
import Track.Parser (parseTrack)

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
                    refInput runningRef
                        >>> player
                        >>> void (onEvent $ action $ writeBChan eventChan)
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
