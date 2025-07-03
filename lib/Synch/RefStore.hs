module Synch.RefStore
    ( RefStore (..)
    ) where

import Protolude

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)

class Monad m => RefStore m where
    type Ref m :: Type -> Type
    newRef :: a -> m (Ref m a)
    getRef :: Ref m a -> m a
    setRef :: Ref m a -> a -> m ()
    modifyRef :: Ref m a -> (a -> a) -> m a

instance RefStore IO where
    type Ref IO = IORef
    newRef = newIORef
    getRef = readIORef
    setRef = writeIORef
    modifyRef r f = do
        modifyIORef' r f
        getRef r

instance RefStore (ST s) where
    type Ref (ST s) = STRef s
    newRef = newSTRef
    getRef = readSTRef
    setRef = writeSTRef
    modifyRef r f = do
        modifySTRef' r f
        getRef r
