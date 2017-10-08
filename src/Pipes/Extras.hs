{-# LANGUAGE RankNTypes, Trustworthy #-}

-- | Unassorted utilities for @pipes@

module Pipes.Extras (
    -- * ArrowChoice
      arr
    , left
    , right
    , (+++)

    -- * Lenses
    , input
    , output

    -- * Fun
    , check
    , delay
    , progress

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

    -- * Church encodings
    , toProxy
    , fromProxy
    ) where

import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Control.Foldl (purely, impurely, Fold, FoldM)
import Pipes
import Pipes.Core (request, respond, (>\\), (//>))
import Pipes.Internal (Proxy(..))
import Control.Lens (Prism, withPrism, _Left, _Right)
import qualified Pipes.Prelude as Pipes

-- | Like 'Control.Arrow.arr' from 'Control.Arrow.Arrow'
arr :: Monad m => (a -> b) -> Pipe a b m r
arr = Pipes.map
{-# INLINABLE arr #-}

select :: Monad m => (b -> t) -> (s -> Either t a) -> Pipe a b m r -> Pipe s t m r
select inj proj pipe = await' >~ for pipe yield'
    where
    yield' b = yield (inj b)
    await' = do
        s <- await
        case proj s of
            Left t -> do
                yield t 
                await'
            Right a -> return a
{-# INLINABLE select #-}

select' :: Monad m => Prism s t a b -> Pipe a b m r -> Pipe s t m r
select' prism = withPrism prism select
{-# INLINABLE select' #-}

-- | Like 'Control.Arrow.left' from 'Control.Arrow.ArrowChoice'
left :: Monad m => Pipe a b m r -> Pipe (Either a x) (Either b x) m r
left = select' _Left
{-# INLINABLE left #-}

-- | Like 'Control.Arrow.right' from 'Control.Arrow.ArrowChoice'
right :: Monad m => Pipe a b m r -> Pipe (Either x a) (Either x b) m r
right = select' _Right
{-# INLINABLE right #-}


{-| Like ('Control.Arrow.+++') from 'Control.Arrow.ArrowChoice'

> pL +++ pR = left pL >-> right pR
-}
(+++)
    :: Monad m
    => Pipe a b m r -> Pipe c d m r -> Pipe (Either a c) (Either b d) m r
pL +++ pR = left pL >-> right pR
{-# INLINABLE (+++) #-}

type Setter s t a b = (a -> Identity b) -> (s -> Identity t)

{-| It helps to think in terms of the following simpler types:

> input :: Monad m => Setter' (Consumer a   m r) a
> input :: Monad m => Setter' (Pipe     a b m r) a

    Note: This only works with @lens@ and not @lens-family-core@
-}
input :: Monad m => Setter (Proxy x' b y' y m r) (Proxy x' a y' y m r) a b
input k p = Identity (request' >\\ p)
  where
    request' a' = fmap (\a -> runIdentity (k a)) (request a')
{-# INLINABLE input #-}

{-| It helps to think in terms of the following simpler types:

> output :: Monad m => Setter' (Producer b m r) b
> output :: Monad m => Setter' (Pipe   a b m r) b

    Note: This only works with @lens@ and not @lens-family-core@
-}
output :: Monad m => Setter (Proxy x' x y' a m r) (Proxy x' x y' b m r) a b
output k p = Identity (p //> respond')
  where
    respond' a = respond (runIdentity (k a))
{-# INLINABLE output #-}

{-| Ask whether or not to let values pass through

>>> runEffect $ each [1..3] >-> check >-> Pipes.print
Allow <1> [Y/n]?
y<Enter>
1
Allow <2> [Y/n]?
no<Enter>
Allow <3> [Y/n]?
YES<Enter>
3

-}
check :: Show a => Pipe a a IO r
check = Pipes.filterM $ \a -> do
    let prompt = do
            putStrLn ("Allow <"  ++ show a ++ "> [Y/n]?")
            str <- getLine
            case map toLower str of
                ""    -> return True
                "y"   -> return True
                "yes" -> return True
                "n"   -> return False
                "no"  -> return False
                _     -> do
                    putStrLn "Please enter (y)es or (n)o."
                    prompt
    prompt
{-# INLINABLE check #-}

{-| Display a progress bar

    This is very simple and only works if nothing else writes to the terminal

    Try this:

>>> runEffect $ each [1..] >-> progress >-> delay 0.1 >-> Pipes.Prelude.drain
-}
progress :: Pipe a a IO r
progress = go (0 :: Integer)
  where
    go n = do
        let str = bar n ++ " " ++ show n
        lift $ putStr str
        a <- await
        yield a
        lift $ putStr (replicate (length str) '\b')
        go (n + 1)
    bar n = case n `mod` 4 of
        0 -> "|"
        1 -> "/"
        2 -> "-"
        _ -> "\\"
{-# INLINABLE progress #-}

-- | Add a delay (in seconds) between each element
delay :: Double -> Pipe a a IO r
delay seconds = for cat $ \a -> do
    yield a
    lift $ threadDelay (truncate (seconds * 1000000))
{-# INLINABLE delay #-}

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

-- | Build a `Proxy` from its church encoding
toProxy
    ::  Monad n
    =>  (   forall m
        .   Monad m
        =>  (a' -> (a  -> m r) -> m r)
        ->  (b  -> (b' -> m r) -> m r)
        ->  m r
        )
    ->  Proxy a' a b' b n r
toProxy k = k
    (\a' fa  -> request a' >>= fa )
    (\b  fb' -> respond b  >>= fb')

-- | Convert a `Proxy` to its church encoding
fromProxy
    ::  Monad m
    =>  Proxy a' a b' b m r
    ->  (a' -> (a  -> m r) -> m r)
    ->  (b  -> (b' -> m r) -> m r)
    ->  m r
fromProxy p request' respond' = case p of
    Request a' fa  -> do
        request' a' (\a  -> fromProxy (fa  a ) request' respond')
    Respond b  fb' -> do
        respond' b  (\b' -> fromProxy (fb' b') request' respond')
    M          m   -> do
        p' <- m
        fromProxy p' request' respond'
    Pure    r      -> return r
