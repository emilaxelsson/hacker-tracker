module Synch.System
    ( System (..)
    , Control (start, stop)
    , stubbornlyRunning
    , newControl
    , execSystem
    , execSystemForever
    ) where

import Data.Fixed (Fixed (..), Pico)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Protolude hiding (empty)

-- | An interactive system
--
-- @`System` a@ can be thought of as an infinite stream of @a@ values with interleaved IO:
-- @IO (a, IO (a, IO (a, ...)))@
newtype System m a = System {unSystem :: m (m a)}

instance Functor f => Functor (System f) where
    fmap f (System sys) = System $ fmap (fmap f) sys

instance Monad m => Applicative (System m) where
    pure = System . return . return
    System initf <*> System inita = System $ do
        nextf <- initf
        nexta <- inita
        return $ nextf <*> nexta

-- | System controls
data Control = Control
    { start :: IO ()
    -- ^ Start the system
    , stop :: IO ()
    -- ^ Pause the system
    , getPermissionToRun :: IO Bool
    -- ^ Internal operation used to achieve system pause. When the system is running, this
    -- operation should return immediately with with value 'False'. When the system is
    -- paused this operation should block (for as long as the pause lasts) and then return
    -- 'True'.
    }

stubbornlyRunning :: Control
stubbornlyRunning = Control
    { start = return ()
    , stop = return ()
    , getPermissionToRun = return False
    }

newControl :: IO Control
newControl = do
    sem <- newEmptyMVar
    let start = void $ tryPutMVar sem ()
    let stop = void $ tryTakeMVar sem
    let getPermissionToRun = do
            empty <- isEmptyMVar sem
            readMVar sem
            return empty

    return Control{start, stop, getPermissionToRun}

-- |
-- >>> picoToMillis 0.001
-- 1
-- >>> picoToMillis 12.3456
-- 12345
picoToMillis :: Pico -> Int
picoToMillis (MkFixed i) = fromInteger $ div i 1_000_000_000

-- | Run a system for as long as it returns 'True'
execSystem
    :: Int
    -- ^ Resolution (milliseconds per tick)
    -> Control
    -> System IO Bool
    -> IO ()
execSystem millisPerTick Control{getPermissionToRun} (System sys) = do
    next <- sys

    ticksRef <- newIORef (0 :: Int)
    startTimeRef <- newIORef =<< getCurrentTime

    let go False = return ()
        go True = do
            wasBlocked <- getPermissionToRun
            when wasBlocked $ do
                writeIORef ticksRef 0
                writeIORef startTimeRef =<< getCurrentTime

            numberOfTicks <- readIORef ticksRef
            startTime <- readIORef startTimeRef
            currentTime <- getCurrentTime

            let realElapsedMillis =
                    picoToMillis $ nominalDiffTimeToSeconds $ diffUTCTime currentTime startTime
            let tickMillis = numberOfTicks * millisPerTick
            let slack = tickMillis - realElapsedMillis

            when (slack > 0) $ do
                threadDelay (slack * 1000)

            keepGoing <- next

            modifyIORef' ticksRef (+ 1)

            go keepGoing

    go True

-- | Run a system indefinitely
execSystemForever
    :: Int
    -- ^ Resolution (milliseconds per tick)
    -> Control
    -> System IO ()
    -> IO ()
execSystemForever millisPerTick control = execSystem millisPerTick control . fmap (const True)
