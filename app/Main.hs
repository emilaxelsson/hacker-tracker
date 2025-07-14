module Main (main) where

import Brick.BChan (newBChan, writeBChan)
import Control.Arrow ((>>>))
import Control.Monad (fail)
import Data.IORef (newIORef, writeIORef)
import Data.Text qualified as Text
import Player (player)
import Player.Schedule (PlayerConfig (..), scheduleTrack)
import Protolude
import Synch (action, onEvent, refInput, runSF)
import Synch.System (execSystemForever)
import TUI (PlayerCommand (..), trackerMain)
import Track.Parser (parseTrack)

main :: IO ()
main = do
    let path :: FilePath
        path = "tests/test-track.md"

    let playerConfig :: PlayerConfig
        playerConfig =
            PlayerConfig
                { millisPerTick = 15
                , sourceFile = path
                }

    track <- readFile path
    ast <- either (fail . show) return $ parseTrack track
    schedule <- either (fail . Text.unpack) return $ scheduleTrack playerConfig ast

    print ast
    print schedule

    eventChan <- newBChan 1
    runningRef <- newIORef False

    bracket
        ( forkIO $
            execSystemForever (millisPerTick playerConfig) $
                runSF $
                    refInput runningRef
                        >>> player playerConfig schedule
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
