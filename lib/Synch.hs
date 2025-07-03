{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecursiveDo #-}

module Synch
    ( SF (..)
    , runSF
    , evalSF
    , stream
    , Event
    , never
    , switch
    , switch_
    , whenA
    , action
    , delay
    , counter
    , rangeCounter
    , latch
    , everyN
    , refInput
    ) where

import Control.Arrow
    ( Arrow (..)
    , ArrowChoice (..)
    , ArrowLoop (..)
    , Kleisli (..)
    , returnA
    , (>>>)
    )
import Control.Category (Category (..))
import Control.Monad.Fix (MonadFix)
import Protolude hiding (first, second, (.))
import Synch.RefStore
import Synch.System
import Prelude (error)

-- | Synchronous stream transformer
newtype SF m a b = SF (m (Kleisli m a b))

instance Functor f => Functor (SF f a) where
    fmap f (SF init) = SF $ fmap (fmap f) init

instance Monad m => Category (SF m) where
    id = SF $ pure id

    SF init2 . SF init1 = SF $ (.) <$> init2 <*> init1

instance Monad m => Arrow (SF m) where
    arr = SF . return . arr
    first (SF init) = SF $ first <$> init

instance Monad m => ArrowChoice (SF m) where
    left (SF init) = SF $ left <$> init

instance (RefStore m, MonadFix m) => ArrowLoop (SF m) where
    loop (SF init) = SF $ loop <$> init

-- | Run a synchronous stream transformer as a 'System'
runSF :: Monad m => SF m () b -> System m b
runSF (SF init) = System $ do
    f <- init
    return $ runKleisli f ()

-- | Evaluate a synchronous stream transformer using lists as input and output
evalSF :: forall a b. (forall m. RefStore m => SF m a b) -> [a] -> [b]
evalSF sf input = runST m
  where
    m :: ST s [b]
    m = do
        let SF init = sf
        next <- init

        let go [] bs = return $ reverse bs
            go (a : as) bs = do
                b <- runKleisli next a
                go as (b : bs)

        go input []

stream :: Monad m => (a -> m b) -> m (Kleisli m a b)
stream = return . Kleisli

type Event = Maybe

never :: Event a
never = Nothing

-- Decent implementation in that `forM_ event ...` is never executed again after the
-- switch. However, the reference will live on forever and be read in each step. This is a
-- problem in case of many nested switches (e.g. an infinite loop that flip-flops between
-- two behaviors). The solution is probably to somehow build switching into `SF`.
switch :: (RefStore m, MonadFix m) => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
switch (SF init) k = SF $ mdo
    next <- init
    nextRef <- newRef $ Kleisli $ \a -> do
        (b, event) <- runKleisli next a
        forM_ event $ \c -> do
            let SF newInit = k c
            newNext <- newInit
            setRef nextRef newNext
        return b

    stream $ \a -> do
        next' <- getRef nextRef
        runKleisli next' a

switch_ :: (RefStore m, MonadFix m) => SF m a (b, Bool) -> SF m a b -> SF m a b
switch_ sf1 sf2 =
    switch
        (sf1 >>> second (arr guard))
        (const sf2)

whenA :: Monad m => SF m () Bool -> SF m a b -> SF m a (Event b)
whenA cond sf = proc a -> do
    c <- cond -< ()
    if c
        then (Just <$> sf) -< a
        else returnA -< Nothing

action :: Monad m => (a -> m b) -> SF m a b
action = SF . return . Kleisli

delay :: RefStore m => a -> SF m a a
delay init = SF $ do
    r <- newRef init
    stream $ \a -> do
        a' <- getRef r
        setRef r a
        return a'

counter :: RefStore m => SF m Bool Int
counter = SF $ do
    cnt <- newRef 0
    stream $ \tick -> do
        c <- getRef cnt
        when tick $
            void $
                setRef cnt (c + 1)
        return c

rangeCounter :: RefStore m => Int -> Int -> SF m () Int
rangeCounter low high
    | low > high = error $ "rangeCounter: invalid range " ++ show (low, high)
    | otherwise = SF $ do
        cnt <- newRef low
        stream $ const $ do
            c <- getRef cnt
            void $
                if c == high
                    then setRef cnt low
                    else setRef cnt (c + 1)
            return c

latch :: RefStore m => a -> SF m (Event a) a
latch init = SF $ do
    r <- newRef init
    stream $ \e -> do
        forM_ e $ setRef r
        getRef r

-- | Slow down a system by only ticking it every nth time
everyN :: RefStore m => Int -> SF m a b -> SF m a (Event b)
everyN n _
    | n <= 0 = error $ "every: expected positive number, got " ++ show n
everyN n (SF init) = SF $ do
    next <- init
    c <- newRef 0
    stream $ \a -> do
        count <- getRef c
        if count == n
            then do
                setRef c 0
                Just <$> runKleisli next a
            else do
                setRef c (count + 1)
                return Nothing

refInput :: RefStore m => Ref m a -> SF m () a
refInput = action . const . getRef
