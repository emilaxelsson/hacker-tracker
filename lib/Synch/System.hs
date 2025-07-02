module Synch.System
    ( System (..)
    , execSystem
    , execSystemForever
    ) where

import Data.Fixed (Fixed (..), Pico)
import Data.IORef (modifyIORef', newIORef, readIORef)
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
    -> System IO Bool
    -> IO ()
execSystem millisPerTick (System sys) = do
    next <- sys

    ticksRef <- newIORef (0 :: Int)
    startTime <- getCurrentTime

    let go False = return ()
        go True = do
            numberOfTicks <- readIORef ticksRef
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
    -> System IO ()
    -> IO ()
execSystemForever millisPerTick = execSystem millisPerTick . fmap (const True)
