-- |
-- Stability: experimental
module Data.Conduit.Resumable (
    -- * ResumableSource
    ResumableSource,
    newResumableSource,
    ($$+),
    ($$++),
    ($$+-),
    finishResumableSource,

    -- * ResumableSink
    ResumableSink,
    newResumableSink,
    (+$$),
    (++$$),
    (-+$$),
    finishResumableSink,

    -- * ResumableConduit
    ResumableConduit,
    newResumableConduit,
    (=$+),
    (=$++),
    (=$+-),
    finishResumableConduit,
) where

import Control.Monad
import Data.Conduit.Internal
import Data.Void

-- ResumableSource (same fixity as $$)
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-

-- ResumableSink (same fixity as $$)
infixr 0 +$$
infixr 0 ++$$
infixr 0 -+$$

-- Not implemented yet
-- ResumableConduit left fusion (same fixity as $=)
-- infixl 1 +$=
-- infixl 1 ++$=
-- infixl 1 -+$=

-- ResumableConduit right fusion (same fixity as =$ and =$=)
infixr 2 =$+
infixr 2 =$++
infixr 2 =$+-

------------------------------------------------------------------------
-- Bootstrap
--
-- These are defined in "Data.Conduit", but are defined again here
-- to avoid circular imports.

infixr 0 $$
infixr 2 =$=

($$) :: Monad m => Source m a -> Sink a m b -> m b
($$) src sink = do
    (rsrc, res) <- src $$+ sink
    rsrc $$+- return ()
    return res
{-# INLINE ($$) #-}

(=$=) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
(=$=) (ConduitM left) (ConduitM right) = ConduitM $ pipeL left right
{-# INLINE (=$=) #-}

------------------------------------------------------------------------
-- ResumableSource
--
-- NOTE: $$ is defined in terms of $$+ and $$+-

-- ResumableSource is defined in Data.Conduit.Internal:
-- data ResumableSource m o = ResumableSource (Source m o) (m ())

-- | Convert a 'Source' into a 'ResumableSource' so it can be used with '$$++'.
newResumableSource :: Monad m => Source m o -> ResumableSource m o
newResumableSource src = ResumableSource src (return ())

-- | The connect-and-resume operator. This does not close the @Source@, but
-- instead returns it to be used again. This allows a @Source@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Mnemonic: connect + do more.
--
-- Since 0.5.0
($$+) :: Monad m => Source m a -> Sink a m b -> m (ResumableSource m a, b)
($$+) src sink = newResumableSource src $$++ sink
{-# INLINE ($$+) #-}

-- | Continue processing after usage of @$$+@.
--
-- Since 0.5.0
($$++) :: Monad m => ResumableSource m a -> Sink a m b -> m (ResumableSource m a, b)
($$++) = connectResume
{-# INLINE ($$++) #-}

-- | Complete processing of a @ResumableSource@. This will run the finalizer
-- associated with the @ResumableSource@. In order to guarantee process resource
-- finalization, you /must/ use this operator after using @$$+@ and @$$++@.
--
-- Since 0.5.0
($$+-) :: Monad m => ResumableSource m a -> Sink a m b -> m b
($$+-) rsrc sink = do
    (ResumableSource _ final, res) <- connectResume rsrc sink
    final
    return res
{-# INLINE ($$+-) #-}

-- | Finalize a 'ResumableSource' by turning it into a 'Source'.
-- The resulting 'Source' may only be used once.
--
-- This may be used instead of '$$+-' to finalize a 'ResumableSource'.
finishResumableSource :: Monad m => ResumableSource m o -> Source m o
finishResumableSource (ResumableSource (ConduitM p) final) =
    ConduitM $ embedFinalizer final p

-- | Make the given 'Pipe' run the given finalizer when it finishes,
-- unless it overrides this finalizer by calling 'Data.Conduit.yieldOr'.
embedFinalizer :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
embedFinalizer final0 p0 =
    go final0 p0
  where
    go final p =
        case p of
            HaveOutput _ _ _ -> p -- Finalizer overridden.
                                  -- Downstream will call the new finalizer
                                  -- if it doesn't use this output value.
            NeedInput i u -> NeedInput (recurse . i) (recurse . u)
            Done r        -> PipeM (final >> return (Done r))
            PipeM mp      -> PipeM (liftM recurse mp)
            Leftover p' l -> Leftover (recurse p') l
      where
        recurse = go final

------------------------------------------------------------------------
-- ResumableSink

newtype ResumableSink i m r = ResumableSink (Pipe i i Void () m r)

-- | Convert a 'Sink' into a 'ResumableSink' so it can be used with '++$$'.
newResumableSink :: Monad m => Sink i m r -> ResumableSink i m r
newResumableSink (ConduitM sink) = ResumableSink sink
    -- The 'Monad' constraint here currently isn't needed, but if finalization
    -- semantics are changed in the future and we need to add a 'Monad'
    -- constraint here, it will cause less breakage.

-- | Connect a source and a sink, allowing the sink to be fed more data later.
-- Return a 'Right' if the sink completes, or a 'Left' if the source is
-- exhausted and the sink requests more input.
(+$$) :: Monad m
      => Source m i
      -> Sink i m r
      -> m (Either (ResumableSink i m r) r)
(+$$) src sink = src ++$$ newResumableSink sink

-- | Feed more data to a 'ResumableSink'.
(++$$) :: Monad m
       => Source m i
       -> ResumableSink i m r
       -> m (Either (ResumableSink i m r) r)
(++$$) (ConduitM left0) (ResumableSink right0) =
    goRight (return ()) left0 right0
  where
    goRight final left right =
        case right of
            HaveOutput _ _ o  -> absurd o
            NeedInput rp rc   -> goLeft rp rc final left
            Done r            -> final >> return (Right r)
            PipeM mp          -> mp >>= goRight final left
            Leftover p i      -> goRight final (HaveOutput left final i) p

    goLeft rp rc final left =
        case left of
            HaveOutput left' final' o -> goRight final' left' (rp o)
            NeedInput _ lc            -> recurse (lc ())
            Done _                    -> return $ Left $
                                         ResumableSink $ NeedInput rp rc
            PipeM mp                  -> mp >>= recurse
            Leftover p _              -> recurse p
      where
        recurse = goLeft rp rc final

-- | Feed the final data to a 'ResumableSink' and close it.  This must be used
-- after '+$$' and '++$$' so that:
--
--  * The sink sees the end of stream.  The sink never sees
--    'Data.Conduit.await' return 'Nothing' until you do this.
--
--  * The sink can release system resources.
(-+$$) :: Monad m => Source m i -> ResumableSink i m r -> m r
(-+$$) src (ResumableSink rsink) = src $$ ConduitM rsink

-- | Finalize a 'ResumableSink' by turning it into a 'Sink'.
-- The resulting 'Sink' may only be used once.
--
-- This may be used instead of '-+$$' to finalize a 'ResumableSink'.
finishResumableSink :: Monad m => ResumableSink i m r -> Sink i m r
finishResumableSink (ResumableSink p) = ConduitM p

------------------------------------------------------------------------
-- ResumableConduit

data ResumableConduit i m o = ResumableConduit (Pipe i i o () m ()) (m ())

-- | Convert a 'Conduit' into a 'ResumableConduit' so it can be used with '=$++'.
newResumableConduit :: Monad m => Conduit i m o -> ResumableConduit i m o
newResumableConduit (ConduitM p) = ResumableConduit p (return ())

-- | Fuse a conduit behind a consumer, but allow the conduit to be reused after
-- the consumer returns.
--
-- When the source runs out, the stream terminator is sent directly to
-- the consumer, bypassing the conduit.  Some conduits wait for a stream terminator
-- before producing their remaining output, so be sure to use '=$+-'
-- to \"flush\" this data out.
(=$+) :: Monad m
      => Conduit a m b      -- ^ Filters incoming data
      -> ConduitM b c m r   -- ^ Consumer
      -> ConduitM a c m (ResumableConduit a m b, r)
(=$+) conduit sink = newResumableConduit conduit =$++ sink

-- | Continue using a conduit after '=$+'.
(=$++) :: Monad m
       => ResumableConduit a m b  -- ^ Filters incoming data
       -> ConduitM b c m r        -- ^ Consumer
       -> ConduitM a c m (ResumableConduit a m b, r)
(=$++) (ResumableConduit conduit0 final0) (ConduitM sink0) =
    ConduitM $ goSink final0 conduit0 sink0
  where
    goSink final conduit sink =
        case sink of
            HaveOutput p c o  -> HaveOutput (recurse p) (c >> final) o
            NeedInput rp rc   -> goConduit rp rc final conduit
            Done r            -> Done (ResumableConduit conduit final, r)
            PipeM mp          -> PipeM (liftM recurse mp)
            Leftover sink' o  -> goSink final (HaveOutput conduit final o) sink'
      where
        recurse = goSink final conduit

    goConduit rp rc final conduit =
        case conduit of
            HaveOutput conduit' final' o -> goSink final' conduit' (rp o)
            NeedInput left' _lc -> NeedInput (recurse . left')
                                             (goSink final conduit . rc)
               -- Forward EOF to sink, but leave the conduit alone
               -- so it accepts input from the next source.
            Done r              -> goSink (return ()) (Done r) (rc r)
            PipeM mp            -> PipeM (liftM recurse mp)
            Leftover conduit' i -> Leftover (recurse conduit') i
      where
        recurse = goConduit rp rc final

-- | Finalize a 'ResumableConduit' by using it one more time.  It will be
-- closed when the consumer finishes.
(=$+-) :: Monad m
       => ResumableConduit a m b  -- ^ Filters incoming data
       -> ConduitM b c m r        -- ^ Consumer
       -> ConduitM a c m r
(=$+-) rconduit sink = finishResumableConduit rconduit =$= sink

-- | Finalize a 'ResumableConduit' by turning it into a 'Conduit'.
-- The resulting 'Conduit' may only be used once.
--
-- This may be used instead of '=$+-' to finalize a 'ResumableConduit'.
finishResumableConduit :: Monad m => ResumableConduit i m o -> Conduit i m o
finishResumableConduit (ResumableConduit p final) =
    ConduitM $ embedFinalizer final p
