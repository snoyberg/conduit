module Data.Conduit.Internal.Composition
    ( -- * Fusion
      pipe
    , (>+>)
    , (<+<)
      -- * Connect and resume
    , connectResume
    , draw
    , drawCPS
      -- * Execute
    , runPipe
    , runPipeCPS
    , runPipeE
    , closePipe
    ) where

import Data.Conduit.Internal.Pipe
import Data.Conduit.Internal.Primitives
import Data.Void
import Control.Monad (liftM)

-- | Compose a left and right pipe together into a complete pipe. The left pipe
-- will be automatically closed when the right pipe finishes.
--
-- Since 0.5.0
pipe :: Monad m
     => (t2 -> b)
     -> (b -> b')
     -> (b -> [i] -> t1 -> Pipe i k c t3 m r)
     -> (b -> [i] -> a -> Pipe i k c t3 m r)
     -> Pipe i j b' t1 m a
     -> Pipe j k c t2 m b
     -> Pipe i k c t3 m r
pipe onT2 mkB' onT1 onPure =
    pipe'
  where
    pipe' up (Pure js0 b) =
        -- This is the tricky bit. We need to ensure that downstream closes
        -- before we do. Ideally, this would be expressed in the type system
        -- itself, but such a construction is more clumsy to use.
        Empty $ \_ _ -> close js0 up
      where
        b' = mkB' b
        -- FIXME remove duplication with runPipe
        close _js (Pure is a) = onPure b is a
        close js (M m) = M (liftM (close js) m)
        -- We need to make sure that the leftovers are only provided once.
        close js (Yield _ done _) = close [] (done js b') -- FIXME analyze behavior of leftovers
        close js (Empty done) = close [] (done js b')
        close js (Await more done) = Await (close js . more) (close js done)
        close js (Check _ done) = close [] (done js b')
        close _js (Terminate is t) = onT1 b is t
    pipe' up (M m) = M (liftM (pipe' up) m)
    pipe' up (Yield more done o) = Yield (pipe' up more) (pipe' up .: done) o
    pipe' up (Empty done) = Empty (pipe' up .: done)
    pipe' up0 (Await moreD doneD) =
        go up0
      where
        go up@Pure{} = pipe' up doneD
        go (M m) = M (liftM go m)
        go (Yield moreU doneU o) = pipe' (Check moreU doneU) (moreD o)
        go up@Empty{} = pipe' up doneD
        go (Await moreU doneU) = Await (go . moreU) (go doneU)
        go (Check moreU _) = go moreU
        go up@Terminate{} = pipe' up doneD
    pipe' up (Check more done) = Check (pipe' up more) (pipe' up .: done)
    pipe' up (Terminate is t) = pipe' up (Pure is $ onT2 t)

infixr 9 <+<
infixl 9 >+>

-- | Fuse together two @Pipe@s, connecting the output from the left to the
-- input of the right.
--
-- Notice that the /leftover/ parameter for the @Pipe@s must be @Void@. This
-- ensures that there is no accidental data loss of leftovers during fusion. If
-- you have a @Pipe@ with leftovers, you must first call 'injectLeftovers'.
--
-- Since 0.5.0
(>+>) :: Monad m
      => Pipe i j b t m a
      -> Pipe j k c b m b
      -> Pipe i k c t m a
(>+>) = pipe id id (const Terminate) (const Pure)
{-# INLINE (>+>) #-}

-- | Same as '>+>', but reverse the order of the arguments.
--
-- Since 0.5.0
(<+<) :: Monad m
      => Pipe j k c b m b
      -> Pipe i j b t m a
      -> Pipe i k c t m a
(<+<) = flip (>+>)
{-# INLINE (<+<) #-}

-- | Connect a @Source@ to a @Sink@ until the latter closes. Returns both the
-- most recent state of the @Source@ and the result of the @Sink@.
--
-- We use a @ResumableSource@ to keep track of the most recent finalizer
-- provided by the @Source@.
--
-- Since 0.5.0
connectResume :: Monad m
              => Pipe () o () () m ()
              -> Pipe o Void () Void m r
              -> m (Pipe () o () () m (), r)
connectResume up =
    go
  where
    go (Pure [] r) = return (up, r)
    go (Pure is r) = return (mapM_ tryYield is >> up, r)
    go (M m) = m >>= go
    go (Yield _ _ o) = absurd o
    go (Check _ done) = go $ done [] ()
    go (Empty done) = go $ done [] ()
    go (Await more done) =
        drawCPS
            (\up' o -> connectResume up' (more o))
            (\up' -> do
                Right res <- runPipeE done
                closePipe up'
                return (return (), res)
                )
            up
    go (Terminate _ t) = absurd t

drawCPS :: Monad m
        => (Pipe () o () t m () -> o -> m a)
        -> (Pipe () o () t m () -> m a)
        -> Pipe () o () t m ()
        -> m a
drawCPS provide done =
    go
  where
    go x@Pure{} = done x
    go (M m) = m >>= go
    go (Yield more done' o) = provide (Check more done') o
    go (Empty done') = done $ done' [] () -- ensure correct ordering in connectResume
    go (Check more _) = go more
    go x@Terminate{} = done x
    go (Await _ none) = go none

draw :: Monad m => Pipe () o () t m () -> m (Pipe () o () t m (), Maybe o)
draw = drawCPS (\p o -> return (p, Just o)) (\p -> return (p, Nothing))

-- | Run a pipe to conclusion.
runPipeCPS :: Monad m
           => d
           -> ([i] -> t -> m a)
           -> ([i] -> r -> m a)
           -> Pipe i o d t m r
           -> m a
runPipeCPS d onT onR =
    go
  where
    go (Pure is r) = onR is r
    go (M m) = m >>= go
    go (Yield _ done _) = go (done [] d)
    go (Empty done) = go (done [] d)
    go (Await _ none) = go none
    go (Check _ done) = go (done [] d)
    go (Terminate is t) = onT is t

-- | Run a pipeline until processing completes.
--
-- Since 0.5.0
runPipe :: Monad m
        => Pipe i o () Void m r
        -> m r
runPipe = runPipeCPS () (const absurd) (const return)

runPipeE :: Monad m
        => Pipe i o () t m r
        -> m (Either t r)
runPipeE = runPipeCPS () (const $ return . Left) (const $ return . Right)

closePipe :: Monad m
        => Pipe i o () t m r
        -> m ()
closePipe = runPipeCPS () (\_ _ -> return ()) (\_ _ -> return ())
