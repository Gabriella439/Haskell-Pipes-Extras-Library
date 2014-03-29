{-# LANGUAGE RankNTypes, Trustworthy #-}

-- | Unassorted utilities for @pipes@

module Pipes.Extras (
      scan1
    , scan1M
    , scan1i
    , scan1iM
    ) where

import Pipes
import qualified Pipes.Prelude as P

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
  P.scan step (begin initial) done
{-# INLINABLE scan1 #-}

-- | Strict, monadic left scan without explicit initial state
scan1M :: Monad m => (x -> a -> m x) -> (a -> m x) -> (x -> m b) -> Pipe a b m r
scan1M step begin done = do
  initial <- await
  P.scanM step (begin initial) done
{-# INLINABLE scan1M #-}
