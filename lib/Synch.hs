{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecursiveDo #-}

module Synch
    ( id
    , SF (..)
    , runSF
    , evalSF
    , stream
    , Event
    , switch
    , switch_
    , action
    , delay
    , counter
    , rangeCounter
    , latch
    , onChange
    , positiveEdge
    , onEvent
    , filterSF
    , everyN
    , refInput
    ) where

import Control.Arrow
    ( Arrow (..)
    , ArrowChoice (..)
    , ArrowLoop (..)
    , Kleisli (..)
    , (<<<)
    , (>>>)
    )
import Control.Category (Category (..))
import Control.Monad.Fix (MonadFix)
import Protolude hiding (first, pred, second, (.))
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

-- | Useful function for constructing an 'SF'
--
-- >>> evalSF (SF $ stream return) [1,2,3]
-- [1,2,3]
stream :: Monad m => (a -> m b) -> m (Kleisli m a b)
stream = return . Kleisli

type Event = Maybe

-- The implementation of `switch` is decent in that `forM_ event ...` is never executed
-- again after the switch. However, the reference will live on forever and be read in each
-- step. This is a problem in case of many nested switches (e.g. an infinite loop that
-- flip-flops between two behaviors). The solution is probably to somehow build switching
-- into `SF`.

-- | Note: leaks memory
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

-- | Note: leaks memory
switch_ :: (RefStore m, MonadFix m) => SF m a (b, Bool) -> SF m a b -> SF m a b
switch_ sf1 sf2 =
    switch
        (sf1 >>> second (arr guard))
        (const sf2)

-- | Lift a monadic action to a signal function
action :: Monad m => (a -> m b) -> SF m a b
action = SF . stream

-- | Delay a signal by one time step
--
-- >>> evalSF (delay 0) [1,2,3]
-- [0,1,2]
delay
    :: RefStore m
    => a
    -- ^ Initial value
    -> SF m a a
delay init = SF $ do
    r <- newRef init
    stream $ \a -> do
        a' <- getRef r
        setRef r a
        return a'

-- | Count the number of incoming 'True' values
--
-- >>> evalSF counter [False, False, True, True, False, True]
-- [0,0,1,2,2,3]
counter :: (RefStore m, Integral n) => SF m Bool n
counter = SF $ do
    cnt <- newRef 0
    stream $ \tick -> do
        c <- getRef cnt
        if tick
            then do
                let c' = c + 1
                setRef cnt c'
                return c'
            else
                return c

-- | Count continuously from lower to upper bound (inclusive)
--
-- >>> evalSF (rangeCounter 3 6) (replicate 10 ())
-- [3,4,5,6,3,4,5,6,3,4]
rangeCounter
    :: RefStore m
    => Int
    -- ^ Lower bound
    -> Int
    -- ^ Upper bound
    -> SF m () Int
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

-- | Hold the value of each event until the next event
--
-- >>> evalSF (latch 0) [Nothing, Nothing, Just 5, Nothing, Nothing, Just 6, Just 7]
-- [0,0,5,5,5,6,7]
latch
    :: RefStore m
    => a
    -- ^ Initial value
    -> SF m (Event a) a
latch init = SF $ do
    r <- newRef init
    stream $ \e -> do
        forM_ e $ setRef r
        getRef r

-- | Emit an event when the input value changes compared to previous cycle
--
-- >>> evalSF onChange [1,1,1,1,4,4,4,4,5]
-- [Just 1,Nothing,Nothing,Nothing,Just 4,Nothing,Nothing,Nothing,Just 5]
onChange :: (RefStore m, Eq a) => SF m a (Event a)
onChange = proc a -> do
    let ja = Just a
    prev <- delay Nothing -< ja
    arr (\(x, y) -> guard (x /= y) >> y) -< (prev, ja)

-- | Emit an event when the changes from 'False' to 'True'
--
-- >>> evalSF positiveEdge [False,False,True,True,False,True]
-- [Nothing,Nothing,Just (),Nothing,Nothing,Just ()]
positiveEdge :: RefStore m => SF m Bool (Event ())
positiveEdge = proc a -> do
    prev <- delay False -< a
    arr (\(x, y) -> guard (not x && y)) -< (prev, a)

-- | Run the provided signal function on each input event
onEvent :: Monad m => SF m a b -> SF m (Event a) (Event b)
onEvent sf = proc aev -> case aev of
    Just a -> arr Just <<< sf -< a
    Nothing -> id -< Nothing

-- | Keep only the values that satisfy the given predicate
--
-- >>> evalSF (filterSF even) [1..6]
-- [Nothing,Just 2,Nothing,Just 4,Nothing,Just 6]
filterSF :: Monad m => (a -> Bool) -> SF m a (Event a)
filterSF pred = arr (\a -> guard (pred a) >> return a)

-- | Emit an event every nth time step
--
-- >>> evalSF (everyN 3) (replicate 10 ())
-- [Nothing,Nothing,Just (),Nothing,Nothing,Just (),Nothing,Nothing,Just (),Nothing]
everyN :: RefStore m => Int -> SF m () (Event ())
everyN n
    | n <= 0 = error $ "everyN expected positive number, got " ++ show n
    | otherwise = rangeCounter 1 n >>> filterSF (== n) >>> arr void

-- | Make a signal function that pulls values from a reference
refInput :: RefStore m => Ref m a -> SF m () a
refInput = action . const . getRef
