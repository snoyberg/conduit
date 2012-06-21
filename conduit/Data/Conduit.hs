module Data.Conduit
    ( -- * Conduit interface
      -- $conduitInterface
      Source
    , Conduit
    , Sink
      -- ** Connect/fuse
      -- $lifecycle
    , ($$)
    , ($=)
    , (=$)
    , (=$=)

      -- * Pipe interface
      -- $pipeInterface
    , Pipe
    , (>+>)
    , (<+<)
    , runPipe
    , injectLeftovers

      -- * Primitives
      -- $primitives
    , await
    , awaitE
    , awaitForever
    , yield
    , yieldOr
    , leftover
      -- ** Finalization
    , bracketP
    , addCleanup

      -- * Connect-and-resume
      -- $connectAndResume
    , ResumableSource
    , ($$+)
    , ($$++)
    , ($$+-)

      -- * Utility functions
    , transPipe
    , mapOutput
    , mapOutputMaybe
    , mapInput
    , withUpstream
      -- ** Generalizing
    , sourceToPipe
    , sinkToPipe
    , conduitToPipe

      -- * Generalized conduit types
      -- $generalizedConduitTypes
    , GSource
    , GSink
    , GLSink
    , GInfSink
    , GLInfSink
    , GConduit
    , GLConduit
    , GInfConduit
    , GLInfConduit

      -- * Flushing
    , Flush (..)

      -- * Convenience re-exports
    , ResourceT
    , MonadResource
    , MonadThrow (..)
    , MonadUnsafeIO (..)
    , runResourceT
    ) where

import Control.Monad.Trans.Resource
import Data.Conduit.Internal
import Data.Void (Void)

{- $conduitInterface

Let's start off with a few simple examples of @conduit@ usage. First, a file
copy utility:

>>> :load Data.Conduit.Binary
>>> runResourceT $ sourceFile "input.txt" $$ sinkFile "output.txt"

@runResourceT@ is a function provided by the @resourcet@ package, and ensures
that resources are properly cleaned up, even in the presence of exceptions. The
type system will enforce that @runResourceT@ is called as needed. The remainder
of this tutorial will not discuss @runResourceT@; please see the documentation
in @resourcet@ for more information.

Looking at the rest of our example, there are three components to understand:
@sourceFile@, @sinkFile@, and the @$$@ operator (called \"connect\"). These
represent the most basic building blocks in @conduit@: a @Source@ produces a
stream of values, a @Sink@ consumes such a stream, and @$$@ will combine these
together.

In the case of file copying, there was no value produced by the @Sink@.
However, often times a @Sink@ will produce some result value. For example:

>>> :load Data.Conduit.List
>>> :module +Prelude
>>> sourceList [1..10] $$ fold (+) 0
55

@sourceList@ is a convenience function for turning a list into a @Source@.
@fold@ implements a strict left fold for consuming the input stream.

The next major aspect to the @conduit@ library: the @Conduit@ type.
This type represents a stream /transformer/. In order to use a @Conduit@, we
must /fuse/ it with either a @Source@ or @Sink@. For example:

>>> :load Data.Conduit.List
>>> :module +Prelude
>>> sourceList [1..10] $= Data.Conduit.List.map (+1) $$ consume
[2,3,4,5,6,7,8,9,10,11]

Notice the addition of the @$=@, or /left fuse/ operator. This combines a
@Source@ and a @Conduit@ into a new @Source@, which can then be connected to a
@Sink@ (in this case, @consume@). We can similarly perform /right fusion/ to
combine a @Conduit@ and @Sink@, or /middle fusion/ to combine two @Conduit@s.

A number of very common functions are provided in the "Data.Conduit.List"
module. Many of these functions correspond very closely to standard Haskell
functions.

In addition to connecting and fusing components together, we can also build up
more sophisticated components through monadic composition. For example, to
create a @Sink@ that ignores the first 3 numbers and returns the sum of the
remaining numbers, we can use:

>>> :load Data.Conduit.List
>>> :module +Prelude
>>> sourceList [1..10] $$ Data.Conduit.List.drop 3 >> fold (+) 0
49

In some cases, we might end up consuming more input than we needed, and want to
provide that input to the next component in our monadic chain. We refer to this
as /leftovers/. The simplest example of this is @peek@.

>>> :load Data.Conduit.List
>>> :set -XNoMonomorphismRestriction
>>> :module +Prelude
>>> let sink = do { first <- peek; total <- fold (+) 0; return (first, total) }
>>> sourceList [1..10] $$ sink
(Just 1,55)

Notice that, although we \"consumed\" the first value from the stream via
@peek@, it was still available to @fold@. This idea becomes even more important
when dealing with chunked data such as @ByteString@s or @Text@.

/Final note/: Notice in the types below that @Source@, @Sink@, and @Conduit@
are just type aliases. This will be explained later. Another important aspect
is resource finalization, which will also be covered below.

-}

{- $lifecycle

It is important to understand the lifecycle of our components. Notice that we
can connect or fuse two components together. When we do this, the component
providing output is called /upstream/, and the component consuming this input
is called /downstream/. We can have arbitrarily long chains of such fusion, so
a single component can simultaneously function as /upstream/ and /downstream/.

Each component can be in one of four states of operation at any given time:

* It hasn't yet started operating.

* It is providing output downstream.

* It is waiting for input from upstream.

* It has completed processing.

Let\'s use @sourceFile@ and @sinkFile@ as an example. When we run @sourceFile
"input" $$ sinkFile "output"@, both components begin in the \"not started\"
state. Next, we start running @sinkFile@ (note: we /always/ begin processing on
the downstream component). @sinkFile@ will open up the file, and then wait for
input from upstream.

Next, we\'ll start running @sourceFile@, which will open the file, read some
data from it, and provide it as output downstream. This will be fed to
@sinkFile@ (which was already waiting). @sinkFile@ will write the data to a
file, then ask for more input. This process will continue until @sourceFile@
reaches the end of the input. It will close the file handle and switch to the
completed state. When this happens, @sinkFile@ is sent a signal that no more
input is available. It will then close its file and return a result.

Now let\'s change things up a bit. Suppose we were instead connecting
@sourceFile@ to @take 1@. We start by running @take 1@, which will wait for
some input. We\'ll then start @sourceFile@, which will open the file, read a
chunk, and send it downstream. @take 1@ will take that single chunk and return
it as a result. Once it does this, it has transitioned to the complete state.

We don\'t want to pull any more data from @sourceFile@, as we do not need it.
So instead, we call @sourceFile@\'s finalizer. Each time upstream provides
output, it also provides a finalizer to be run if downstream finishes
processing.

One final case: suppose we connect @sourceFile@ to @return ()@. The latter does
nothing: it immediately switches to the complete state. In this case, we never
even start running @sourceFile@ (it stays in the \"not yet started\" state),
and so no finalization occurs.

So here are the takeaways from the above discussion:

* When upstream completes before downstream, it cleans up all of its resources
  and sends some termination signal. We never think about upstream again. This
  can only occur while downstream is in the \"waiting for input\" state, since
  that is the only time that upstream is called.

* When downstream completes before upstream, we finalize upstream immediately.
  This can only occur when upstream produces output, because that\'s the only
  time when control is passed back to downstream.

* If downstream never awaits for input before it terminates, upstream was never
  started, and therefore it does not need to be finalized.

Note that all of the discussion above applies equally well to chains of
components. If you have an upstream, middle, and downstream component, and
downstream terminates, then the middle component will be finalized, which in
turn will trigger upstream to be finalized. This setup ensures that we always
have prompt resource finalization.

-}

{- $pipeInterface

We discussed three main types in the @conduit@ package: @Source@, @Sink@, and
@Conduit@. In fact, these are all unified into a single type, @Pipe@. This
greatly simplifies the internal workings of this package, and makes it much
easier to build more powerful components from simpler ones. For example, it is
easy to combine a number of simple @Sink@s together to produce a more powerful
@Conduit@. To create a @Conduit@ which drops 3 input elements and doubles the
rest, we could use:

>>> :load Data.Conduit.List
>>> :set -XNoMonomorphismRestriction
>>> let conduit = do { drop 3; map (Prelude.* 2) }
>>> sourceList [1..10] $$ conduit =$ consume
[8,10,12,14,16,18,20]

If we look again at our examples from above, we'll see a few different aspects
to @Pipe@s:

* @Sink@s and @Conduit@s can consume a stream of /input/ values. Both @map@ and
  @fold@ took a stream of @Int@s, while @sinkFile@ took a stream of
  @ByteString@s.

* @Source@s and @Conduit@s can produce a stream of /output/ values. @sourceFile@
  produced a stream of @ByteString@s, which was then consumed by @sinkFile@. This
  is an important point in @conduit@: the output of the left-hand pipe (a.k.a.,
  /upstream/) must match the input of the right-hand pipe (a.k.a., /downstream/).

* All @Pipe@s have some underlying @Monad@. The @sourceFile@ and @sinkFile@
  functions needed to use @MonadResource@ from @resourcet@ to get exception
  handling, but our other functions could live in any monad. Since @Pipe@
  provides a @MonadTrans@ instance, you can actually lift any action from the
  underlying @Monad@ into your @Pipe@.

* @Sink@s can provide a /result/ type. Our @fold@ returned a final @Int@, while
  @sinkFile@ returned @()@.

A @Pipe@ also exposes two other features as well, not covered by the above
three types:

* Each @Pipe@ has some /leftover/ value. Above, we described a situation where
  the leftover would be identical to the input type. However, @Pipe@ provides a
  type parameter for this instead, so that you can alternatively set the leftover
  type to 'Void', thereby ensuring that a @Pipe@ does not provide any leftover
  values. This is important for ensuring that leftover values aren't accidentally
  discarded.

* Above, we described a situation where only @Sink@s could return results.
  However, sometimes it's advantageous to allow stream producers to also produce
  a result type. We call this the /upstream/ result.

Putting this all together, a @Pipe@ has six type parameters: @Pipe l i o u m r@,
corresponding to each of the bullets above. @Source@, @Conduit@, and @Sink@ are
simply type aliases that restrict one or more of these type parameters to
specific types. For example, both @Source@ and @Conduit@ have @r@ restricted to
@()@, since neither may return a result.

There are two ways that @Pipe@s can be composed: via the @Monad@ instance, and
via fusion. (/Note/: connecting is just a special case of fusion, where the
@Pipe@ is then run. We'll discuss that more later on.) In the @pipes@ package,
these are referred to as /vertical/ and /horizontal/ composition, respectively.
Let's clarify the distinction between these two:

Monadic composition takes two @Pipe@s with the same input and output types, and
combines them into a single @Pipe@. These two @Pipe@s will be run one after the
other, and they will share the same input and output streams. Essentially, the
second @Pipe@ will continue consuming input where the first left off, and the
output streams of each will be concatenated. Any leftover values from the first
@Pipe@ will be fed to the second @Pipe@. Let\'s see a simple example:

>>> :load Data.Conduit.List
>>> sourceList [1..10] $$ do { x <- take 3; y <- take 2; return (x, y) }
([1,2,3],[4,5])

Fusion, on the other hand, will connect the output from an upstream @Pipe@ to
the input of a downstream @Pipe@. The upstream @Pipe@ is required to have a
result type of @()@, since any results it produces are thrown out. This form of
composition produces a new @Pipe@ with the input parameter of the upstream
@Pipe@ and the output and result parameters of the downstream @Pipe@. (For
examples, see the initial examples on this page. Every usage of the connect or
fusion operators is fusion composition.)

/Note/: If you are building a library of @conduit@ functions, it is best to
keep the type signatures as general as possible. For example, even though the
simplest type signature for the @drop@ function would be @Int -> Sink i m ()@,
this would prevent it from being used in construction of @Conduit@s. Instead,
we give it a type signature of @Int -> Pipe l i o u m ()@.

-}

{- $primitives

While @conduit@ provides a number of built-in @Source@s, @Sink@s, and
@Conduit@s, you will almost certainly want to construct some of your own.
Previous versions recommended using the constructors directly. Beginning with
0.5, the recommended approach is to compose existing @Pipe@s into larger ones.

It is certainly possible (and advisable!) to leverage existing @Pipe@s- like
those in "Data.Conduit.List". However, you will often need to go to a lower
level set of @Pipe@s to start your composition. The following few functions
should be sufficient for expressing all constructs besides finalization. Adding
in @bracketP@ and @addCleanup@, you should be able to create any @Pipe@ you
need. (In fact, that's precisely how the remainder of this package is written.)

The three basic operations are /awaiting/, /yielding/, and /leftovers/.
Awaiting asks for a new value from upstream, or returns @Nothing@ if upstream
is done. For example:

>>> :load Data.Conduit.List
>>> sourceList [1..10] $$ await
Just 1

>>> :load Data.Conduit.List
>>> sourceList [] $$ await
Nothing

Similarly, we have a @yield@ function, which provides a value to the downstream
@Pipe@. @yield@ features /auto-termination/: if the downstream @Pipe@ has
already completed processing, the upstream @Pipe@ will stop processing when it
tries to @yield@.

The upshot of this is that you can write code that appears to loop infinitely,
and yet will terminate.

>>> :set -XNoMonomorphismRestriction
>>> let infinite = yield () >> infinite
>>> infinite $$ await
Just ()

Or for something a bit more sophisticated:

>>> let enumFrom' i = yield i >> enumFrom' (succ i)
>>> enumFrom' 1 $$ take 5
[1,2,3,4,5]

The final primitive @Pipe@ is @leftover@. This allows you to return unused
input to be used by the next @Pipe@ in the monadic chain. A simple use case
would be implementing the @peek@ function:

>>> let peek = await >>= maybe (return Nothing) (\x -> leftover x >> return (Just x))
>>> enumFrom' 1 $$ do { mx <- peek; my <- await; mz <- await; return (mx, my, mz) }
(Just 1,Just 1,Just 2)

Note that you should only return leftovers that were previously yielded from
upstream.

-}

{- $connectAndResume

Sometimes, we do not want to force our entire application to live inside the
@Pipe@ monad. It can be convenient to keep normal control flow of our program,
and incrementally apply data from a @Source@ to various @Sink@s. A strong
motivating example for this use case is interleaving multiple @Source@s, such
as combining a @conduit@-powered HTTP server and client into an HTTP proxy.

Normally, when we run a @Pipe@, we get a result and can never run it again.
Connect-and-resume allows us to connect a @Source@ to a @Sink@ until the latter
completes, and then return the current state of the @Source@ to be applied
later. To do so, we introduce three new operators. Let\' start off by
demonstrating them:

>>> :load Data.Conduit.List
>>> (next, x) <- sourceList [1..10] $$+ take 5
>>> Prelude.print x
[1,2,3,4,5]
>>> (next, y) <- next $$++ (isolate 4 =$ fold (Prelude.+) 0)
>>> Prelude.print y
30
>>> next $$+- consume
[10]

-}

{- $generalizedConduitTypes

It's recommended to keep your type signatures as general as possible to
encourage reuse. For example, a theoretical signature for the @head@ function
would be:

> head :: Sink a m (Maybe a)

However, doing so would prevent usage of @head@ from inside a @Conduit@, since
a @Sink@ sets its output type parameter to @Void@. The most general type
signature would instead be:

> head :: Pipe l a o u m (Maybe a)

However, that signature is much more confusing. To bridge this gap, we also
provide some generalized conduit types. They follow a simple naming convention:

* They have the same name as their non-generalized types, with a @G@ prepended.

* If they have leftovers, we add an @L@.

* If they consume the entirety of their input stream and return the upstream
  result, we add @Inf@ to indicate /infinite consumption/.

-}

-- Define fixity of all our operators
infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=
infixr 9 <+<
infixl 9 >+>
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- When either side closes, the other side will immediately be closed as well.
-- If you would like to keep the @Source@ open to be used for another
-- operations, use the connect-and-resume operator '$$+'.
--
-- Since 0.4.0
($$) :: Monad m => Source m a -> Sink a m b -> m b
src $$ sink = do
    (rsrc, res) <- src $$+ sink
    rsrc $$+- return ()
    return res
{-# INLINE ($$) #-}

-- | Left fuse, combining a source and a conduit together into a new source.
--
-- Both the @Source@ and @Conduit@ will be closed when the newly-created
-- @Source@ is closed.
--
-- Leftover data from the @Conduit@ will be discarded.
--
-- Since 0.4.0
($=) :: Monad m => Source m a -> Conduit a m b -> Source m b
($=) = pipeL
{-# INLINE ($=) #-}

-- | Right fuse, combining a conduit and a sink together into a new sink.
--
-- Both the @Conduit@ and @Sink@ will be closed when the newly-created @Sink@
-- is closed.
--
-- Leftover data returned from the @Sink@ will be discarded.
--
-- Since 0.4.0
(=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c
(=$) = pipeL
{-# INLINE (=$) #-}

-- | Fusion operator, combining two @Conduit@s together into a new @Conduit@.
--
-- Both @Conduit@s will be closed when the newly-created @Conduit@ is closed.
--
-- Leftover data returned from the right @Conduit@ will be discarded.
--
-- Since 0.4.0
(=$=) :: Monad m => Conduit a m b -> Conduit b m c -> Conduit a m c
(=$=) = pipeL
{-# INLINE (=$=) #-}


-- | Fuse together two @Pipe@s, connecting the output from the left to the
-- input of the right.
--
-- Notice that the /leftover/ parameter for the @Pipe@s must be @Void@. This
-- ensures that there is no accidental data loss of leftovers during fusion. If
-- you have a @Pipe@ with leftovers, you must first call 'injectLeftovers'. For
-- example:
--
-- >>> :load Data.Conduit.List
-- >>> :set -XNoMonomorphismRestriction
-- >>> let pipe = peek >>= \x -> fold (Prelude.+) 0 >>= \y -> return (x, y)
-- >>> runPipe $ sourceList [1..10] >+> injectLeftovers pipe
-- (Just 1,55)
--
-- Since 0.5.0
(>+>) :: Monad m => Pipe l a b r0 m r1 -> Pipe Void b c r1 m r2 -> Pipe l a c r0 m r2
(>+>) = pipe
{-# INLINE (>+>) #-}

-- | Same as '>+>', but reverse the order of the arguments.
--
-- Since 0.5.0
(<+<) :: Monad m => Pipe Void b c r1 m r2 -> Pipe l a b r0 m r1 -> Pipe l a c r0 m r2
(<+<) = flip pipe
{-# INLINE (<+<) #-}

-- | The connect-and-resume operator. This does not close the @Source@, but
-- instead returns it to be used again. This allows a @Source@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Mnemonic: connect + do more.
--
-- Since 0.5.0
($$+) :: Monad m => Source m a -> Sink a m b -> m (ResumableSource m a, b)
src $$+ sink = connectResume (ResumableSource src (return ())) sink
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
rsrc $$+- sink = do
    (ResumableSource _ final, res) <- connectResume rsrc sink
    final
    return res
{-# INLINE ($$+-) #-}

-- | Provide for a stream of data that can be flushed.
--
-- A number of @Conduit@s (e.g., zlib compression) need the ability to flush
-- the stream at some point. This provides a single wrapper datatype to be used
-- in all such circumstances.
--
-- Since 0.3.0
data Flush a = Chunk a | Flush
    deriving (Show, Eq, Ord)
instance Functor Flush where
    fmap _ Flush = Flush
    fmap f (Chunk a) = Chunk (f a)
