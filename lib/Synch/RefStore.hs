module Synch.RefStore
    ( RefStore (..)
    ) where

import Protolude

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)

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
