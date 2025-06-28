module Player (play) where

import Data.Vector.Storable qualified as VS
import GHC.IO.Handle (hFlush)
import Protolude
import Sound.RtMidi (closePort, defaultOutput, openPort, portCount, portName, sendMessage)

play :: IO ()
play = do
    device <- defaultOutput
    numPorts <- portCount device
    ports <- mapM (portName device) [0 .. numPorts - 1]
    mapM_ print $ zip [0 :: Int ..] ports
    putStr @Text "Select port: "
    hFlush stdout
    selection <- getLine
    Just port <- return $ readMaybe selection
    openPort device port "RtMidi"
    let arp0 = take 12 $ cycle [36] :: [Word8]
    let arp1 = take 12 $ cycle [36] :: [Word8]
    let arp2 = take 12 $ cycle [36] :: [Word8]
    let song = cycle (arp0 ++ arp1 ++ arp2 ++ arp0)

    putStrLn @Text "Playing..."
    sendMessage device (VS.fromList [0xc0, 89, 7])
    forM_ (take 40 song) $ \x -> do
        sendMessage device (VS.fromList [0x90, x, 127])
        threadDelay 300000

    putStrLn @Text "Exiting..."
    closePort device
