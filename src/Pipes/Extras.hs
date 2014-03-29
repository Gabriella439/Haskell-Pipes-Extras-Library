{-# LANGUAGE RankNTypes, Trustworthy #-}

-- | Unassorted utilities for @pipes@

module Pipes.Extras (
    -- * ArrowChoice
      arr
    , left
    , right
    , (+++)

    -- * Foldl Compatibility
    -- $foldl
    , fold
    , foldM
    , scan
    , scanM

    -- * Fold Variations
    -- $variations
    , scan1
    , scan1M
    , scan1i
    , scan1iM
    ) where

import Control.Foldl (purely, impurely, Fold, FoldM)
import Pipes
import qualified Pipes.Prelude as Pipes

-- | Like 'Control.Arrow.arr' from 'Control.Arrow.Arrow'
arr :: Monad m => (a -> b) -> Pipe a b m r
arr = Pipes.map
{-# INLINABLE arr #-}

-- | Like 'Control.Arrow.left' from 'Control.Arrow.ArrowChoice'
left :: Monad m => Pipe a b m r -> Pipe (Either a x) (Either b x) m r
left p = await' >~ for p yield'
  where
    yield' b = yield (Left b)
    await' = do
        e <- await
        case e of
            Left  a -> return a
            Right x -> do
                yield (Right x)
                await'
{-# INLINABLE left #-}

-- | Like 'Control.Arrow.right' from 'Control.Arrow.ArrowChoice'
right :: Monad m => Pipe a b m r -> Pipe (Either x a) (Either x b) m r
right p = await' >~ for p yield'
  where
    yield' b = yield (Right b)
    await' = do
        e <- await
        case e of
            Left  x -> do
                yield (Left x)
                await'
            Right a -> return a
{-# INLINABLE right #-}

{-| Like ('Control.Arrow.+++') from 'Control.Arrow.ArrowChoice'

> pL +++ pR = left pL >-> right pR
-}
(+++)
    :: Monad m
    => Pipe a b m r -> Pipe c d m r -> Pipe (Either a c) (Either b d) m r
pL +++ pR = left pL >-> right pR
{-# INLINABLE (+++) #-}

{- $foldl
    Note that you can already mix the @pipes@ and @foldl@ libraries without
    using @pipes-extras@ just by using:

> import Control.Foldl (purely)
> import Pipes.Prelude (fold)
>
> purely fold :: Monad m => Fold a b -> Producer a m () -> m b

    The following functions are for people who are too lazy to do even that.
-}

-- | Strict fold of the elements of a 'Producer'
fold :: Monad m => Fold a b -> Producer a m () -> m b
fold = purely Pipes.fold
{-# INLINABLE fold #-}

-- | Strict, monadic fold of the elements of a 'Producer'
foldM :: Monad m => FoldM m a b -> Producer a m () -> m b
foldM = impurely Pipes.foldM
{-# INLINABLE foldM #-}

-- | Strict left scan
scan :: Monad m => Fold a b -> Pipe a b m r
scan = purely Pipes.scan
{-# INLINABLE scan #-}

-- | Strict, monadic left scan
scanM :: Monad m => FoldM m a b -> Pipe a b m r
scanM = impurely Pipes.scanM
{-# INLINABLE scanM #-}

{- $variations
    These are minor variations on left folds / scans
-}

{-| Strict, endomorphic left scan without explicit initial state.

> -- Compute exponential moving average
> ema :: (Monad m, Fractional a) => a -> Pipe a a m r
> ema α = scan1i (\last input -> last * α + input * (1 - α))
-}
scan1i :: Monad m => (a -> a -> a) -> Pipe a a m r
scan1i step = scan1 step id id
{-# INLINABLE scan1i #-}

-- | Strict, monadic and endomorphic left scan without explicit initial state
scan1iM :: Monad m => (a -> a -> m a) -> Pipe a a m r
scan1iM step = scan1M step return return
{-# INLINABLE scan1iM #-}

-- | Strict left scan without explicit initial state
scan1 :: Monad m => (x -> a -> x) -> (a -> x) -> (x -> b) -> Pipe a b m r
scan1 step begin done = do
  initial <- await
  Pipes.scan step (begin initial) done
{-# INLINABLE scan1 #-}

-- | Strict, monadic left scan without explicit initial state
scan1M :: Monad m => (x -> a -> m x) -> (a -> m x) -> (x -> m b) -> Pipe a b m r
scan1M step begin done = do
  initial <- await
  Pipes.scanM step (begin initial) done
{-# INLINABLE scan1M #-}
