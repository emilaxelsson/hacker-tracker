{-# LANGUAGE Arrows #-}

module Main (main) where

import Brick.BChan (newBChan, writeBChan)
import Control.Monad (fail)
import Data.HashMap.Strict qualified as HM
import Data.IORef (newIORef, writeIORef)
import Data.Text qualified as Text
import GHC.IO.Handle (hFlush)
import Midi (playNote)
import Player (player)
import Player.Schedule (PlayerConfig (..), scheduleTrack)
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
import Track.Parser (parseTrack)
import Track.Types (Track (..), TrackConfig (..))

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

    track <- readFile path
    ast@Track{config = TrackConfig{instruments}} <-
        either (fail . show) return $ parseTrack track
    let instrumentMap = HM.fromList instruments
    schedule <-
        either (fail . Text.unpack) return $ scheduleTrack playerConfig instrumentMap ast

    print ast
    print schedule

    eventChan <- newBChan 1
    runningRef <- newIORef False

    let playNotes = mapM $ playNote device

    bracket
        ( forkIO $
            execSystemForever (millisPerTick playerConfig) $
                runSF $ proc () -> do
                    input <- refInput runningRef -< ()
                    (appEvent, playEvent) <- player playerConfig schedule -< input
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
