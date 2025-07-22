{-# LANGUAGE Arrows #-}

module Main (main) where

import Brick.BChan (newBChan, writeBChan)
import Control.Monad (fail)
import Data.IORef (newIORef, writeIORef)
import GHC.IO.Handle (hFlush)
import Midi (playNote)
import Player (player)
import Protolude
import Sound.RtMidi
    ( OutputDevice
    , closePort
    , defaultOutput
    , openPort
    , portCount
    , portName
    )
import Synch (action, onEvent, refInput, runSF)
import Synch.System (execSystemForever)
import TUI (PlayerCommand (..), trackerMain)
import Track.AST (Track (..))
import Track.Check (checkPatterns)
import Track.Parser (parseTrack)
import Track.Schedule (PlayerConfig (..), scheduleTrack)

selectMidiPort :: IO OutputDevice
selectMidiPort = do
    device <- defaultOutput
    numPorts <- portCount device
    ports <- mapM (portName device) [0 .. numPorts - 1]
    mapM_ print $ zip [0 :: Int ..] ports
    putStr @Text "Select port: "
    hFlush stdout
    selection <- getLine
    Just port <- return $ readMaybe selection
    openPort device port "RtMidi"
    return device

main :: IO ()
main = do
    device <- selectMidiPort

    let path :: FilePath
        path = "tests/test-track.md"

    let playerConfig :: PlayerConfig
        playerConfig =
            PlayerConfig
                { millisPerTick = 15
                , sourceFile = path
                }

    file <- readFile path
    ast <- either (fail . show) return $ parseTrack file
    checkedPatterns <- either (fail . show) return $ checkPatterns ast
    let scheduledTrack = scheduleTrack playerConfig (config ast) checkedPatterns

    print ast
    print scheduledTrack

    eventChan <- newBChan 1
    runningRef <- newIORef False

    let playNotes = mapM $ playNote device

    bracket
        ( forkIO $
            execSystemForever (millisPerTick playerConfig) $
                runSF $ proc () -> do
                    input <- refInput runningRef -< ()
                    (appEvent, playEvent) <- player playerConfig scheduledTrack -< input
                    onEvent (action $ writeBChan eventChan) -< appEvent
                    void $ onEvent (action playNotes) -< playEvent
        )
        ( \tid -> do
            killThread tid
            closePort device
        )
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
