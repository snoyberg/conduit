module Data.Conduit
    ( -- * Overview
      -- $overview

      -- * Types
      -- $types
      Pipe
    , Source
    , Conduit
    , Sink

      -- * Construction
      -- $construction
    , await
    , awaitE
    , yield
    , yieldOr
    , leftover
      -- ** Finalization
    , bracketP
    , addCleanup

      -- * Utility functions
    , transPipe
    , mapOutput
    , mapOutputMaybe
    , mapInput

      -- * Connect/fuse operators
      -- $fusion
    , ($$)
    , ($$+)
    , ($$++)
    , ($$+-)
    , ($=)
    , (=$)
    , (=$=)

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

{- $overview

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

There is one more major aspect to the @conduit@ library: the @Conduit@ type.
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
Examples will follow.

A number of very common functions are provided in the "Data.Conduit.List"
module. Many of these functions correspond very closely to standard Haskell
functions.

-}

{- $types

We discussed three main types in the @conduit@ package: @Source@, @Sink@, and
@Conduit@. In fact, these are all unified into a single type, @Pipe@. This
greatly simplifies the internal workings of this package, and makes it much
easier to build more powerful components from simpler ones. (For example, it is
easy to combine a number of simple @Sink@s together to produce a more powerful
@Conduit@.)

If we look again at our examples from above, we'll see a few different aspects
to @Pipe@s:

* @Sink@s and @Conduit@s can consume a stream of input values. Both @map@ and
  @fold@ took a stream of @Int@s, while @sinkFile@ took a stream of
  @ByteString@s.

* @Source@s and @Conduit@s can produce a stream of output values. @sourceFile@
  produced a stream of @ByteString@s, which was then consumed by @sinkFile@. This
  is an important point in @conduit@: the output of the left-hand pipe (a.k.a.,
  /upstream/) must match the input of the right-hand pipe (a.k.a., /downstream/).

* All @Pipe@s have some underlying @Monad@. The @sourceFile@ and @sinkFile@
  functions needed to use @MonadResource@ from @resourcet@ to get exception
  handling, but our other functions could live in any monad. Since @Pipe@
  provides a @MonadTrans@ instance, you can actually lift any action from the
  underlying @Monad@ into your @Pipe@.

* @Sink@s can provide a result type. Our @fold@ returned a final @Int@, while
  @sinkFile@ returned @()@.

Putting this all together, a @Pipe@ has four type parameters: @Pipe i o m r@,
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
output streams of each will be concatenated. Let\'s see a simple example:

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

-}

{- $construction

While @conduit@ provides a number of built-in @Source@s, @Sink@s, and
@Conduit@s, you will almost certainly want to construct some of your own. While
previous versions recommended using the constructors directly, beginning with
0.5, the recommended approach is to compose existing @Pipe@s into larger ones.

It is certainly possible (and advisable!) to leverage existing @Pipe@s- like
those in "Data.Conduit.List". However, you will often need to go to a lower
level set of @Pipe@s to start your composition. The following three functions
should be sufficient for expressing all constructs besides finalization. Adding
in @bracketP@ and @addCleanup@, you should be able to create any @Pipe@ you
need. (In fact, that's precisely how the remainder of this package is written.)

The three basic operations are /awaiting/, /yielding/, and /leftovers/.
Awaiting asks for a new value from upstream, or returns @Nothing@ if upstream
is done. For example:

>>> :load Data.Conduit.List
>>> sourceList [1..10] $$ tryAwait
Just 1

>>> :load Data.Conduit.List
>>> sourceList [] $$ tryAwait
Nothing

Similarly, we have a @tryYield@ function. For those familiar with the @pipes@
package, there is a subtle yet important distinction from that package's
@yield@ function. @yield@ features automatic termination, where an upstream
@Pipe@ will stop processing as soon as a downstream @Pipe@ stops. This is not
the case in @conduit@: due to differences to how result values are generated
and how finalization is performed, @conduit@ avoids automatic termination. (See
the section on terminating pipes below for more details.)

The upshot of this is that implementing the equivalent of the following in
@conduit@ results in an infinite loop:

> forever $ yield ()

Therefore, @tryYield@ takes two arguments: a value to yield, and a @Pipe@ to
continue with /if downstream is still accepting input/. To implement the
equivalent of the above, you would write:

>>> let infinite = tryYield () infinite
>>> infinite $$ tryAwait
Just ()

Or for something a bit more sophisticated:

>>> let enumFrom' i = tryYield i $ enumFrom' $ succ i
>>> enumFrom' 1 $$ take 5
[1,2,3,4,5]

Note that you should in general avoid using monadic bind after a call to
@tryYield@, as that will similarly result in code being called after downstream
is closed.

The final primitive @Pipe@ is @leftover@. This allows you to return unused
input to be used by the next @Pipe@ in the monadic chain. A simple use case
would be implementing the @peek@ function:

>>> let peek = tryAwait >>= maybe (return Nothing) (\x -> leftover x >> return (Just x)) :: Pipe i o Prelude.IO (Maybe i)
>>> enumFrom' 1 $$ do { mx <- peek; my <- tryAwait; mz <- tryAwait; return (mx, my, mz) }
(Just 1,Just 1,Just 2)

Note that you should only return leftovers that were previously yielded from
upstream.

-}

{- $terminatingPipes

When you bind two @Pipe@s together monadically, both @Pipe@s are guaranteed to
run. This has two positive ramifications: finalizers are guaranteed to run, and
we are guaranteed to get a result value. However, this has the downside of
requiring explicit management of @Pipe@ termination, as exemplified by the
@tryYield@ examples above.

However, if we were to relax the two requirements above, we could create a
version of @Pipe@ that automatically terminates when either upstream or
downstream terminates. This is precisely what a @TPipe@ provides. Using a
@TPipe@, you can write the simple, infinitely looping versions of some
functions, and have them terminate.

>>> toPipe (mapM_ yield [1..]) $$ tryAwait
Just 1

Note the usage of @toPipe@, which converts a @TPipe@ to a @Pipe@, and the usage
of @yield@ in place of @tryYield@. We can also use @TPipe@s to construct many
@Conduit@s, such as @map@.

>>> let map' f = toPipe $ Control.Monad.forever $ await >>= yield . f
>>> toPipe (mapM_ yield [1..]) $$ map' (+ 1) =$ tryAwait
Just 2

You can also implement some @Sink@s, but only those that return no results.

>>> let mapM_' f = toPipe $ Control.Monad.forever $ await >>= Control.Monad.Trans.Class.lift . f
>>> toPipe (mapM_ yield [1..5]) $$ mapM_' (putStr . show)
12345

-}

{- $fusion

We've already demonstrated how to use most of the connect and fuse operators.
Let's now address some intuition about what they do.

Under the surface, all five operators in this section are implemented by a
single function: 'pipeResume'. At a high level, this function does the
following:

* If the downstream @Pipe@ needs input, it runs the upstream @Pipe@ until it
  produces output.

* If the upstream @Pipe@ has no more output, it indicates this to the
  downstream @Pipe@ until completion.

* If the downstream @Pipe@ is done, it returns the result value from
  downstream, together with the current state of the upstream @Pipe@.

There are other details as well, such as dealing with running monadic actions,
resource finalization, upstream input requirements, and downstream output.
Please see the source of @pipeResume@ for a more detailed analysis. For our
purposes here, the above explanation is sufficient.

It's important to note the type of @pipeResume@:

> pipeResume :: Pipe a b m () -> Pipe b c m r -> Pipe a c m (Pipe a b m (), r)

We'll discuss later why we would want the final state of the upstream @Pipe@ to
be returned, but for now there are two other questions: how do we get rid of
that upstream state, and how do we actually run the @Pipe@?

The former is handled by the 'pipe' function. This function applies
@pipeResume@, runs any finalizers from the upstream @Pipe@, and then returns
the result. Its type is:

> pipe :: Pipe a b m () -> Pipe b c m r -> Pipe a c m r

And in fact, our fusion operators (@$=@, @=$@, and @=$=@) are all synonyms for
this single function. The former two are type restricted to specifically fuse
on the left and right respectively, but technically all usages of either of the
first two can be replaced by the third.

The second question is how we get rid of the surrounding @Pipe@. The answer is
the 'runPipe' function:

> runPipe :: Pipe Void Void m r -> m r

Notice how the input and output types are both set to @Void@. This ensures that
we have a complete pipeline, going from a @Source@ to a @Sink@. We are
guaranteed that the @Pipe@ requires no input stream, and produces no output
stream.

And finally, what's going on with @pipeResume@? It seems silly that we go to
all the effort of retaining that upstream @Pipe@ just to throw it away later.
The reason is so that we can implement the @$$+@ operator (called
connect-and-resume). The idea is that you can incrementally apply a @Source@ to
a number of different @Sink@s, without structuring your entire codebase around
@conduit@.

Most use cases will not require this operator, but certain more complicated
control flows benefit greatly from it. The original use case was simplifying
composition of multiple streams, as occurs when trying to combine a
@conduit@-powered HTTP server and client into an HTTP proxy. For more examples
of usage, see the @warp@ and @http-conduit@ codebases.

-}

infixr 0 $$
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-
infixl 1 $=
infixr 2 =$
infixr 2 =$=


-- | The connect operator, which pulls data from a source and pushes to a sink.
-- When either side closes, the other side will be immediately closed as well.
-- If you would like to keep the @Source@ open to be used for another
-- operations, use the connect-and-resume operator '$$+'.
--
-- Since 0.4.0
($$) :: Monad m => Source m a -> Sink a m b -> m b
src $$ sink = runPipe $ pipe src sink
{-# INLINE ($$) #-}

-- | The connect-and-resume operator. This does not close the @Source@, but
-- instead returns it to be used again. This allows a @Source@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Mnemonic: connect + do more.
--
-- Since 0.4.0
($$+) :: Monad m => Source m a -> Sink a m b -> m (ResumablePipe Void a () m (), b)
src $$+ sink = runPipe $ pipeResume (ResumablePipe src (return ())) sink
{-# INLINE ($$+) #-}

($$++) :: Monad m => ResumablePipe Void a () m () -> Sink a m b -> m (ResumablePipe Void a () m (), b)
rsrc $$++ sink = runPipe $ pipeResume rsrc sink
{-# INLINE ($$++) #-}

($$+-) :: Monad m => ResumablePipe Void a () m () -> Sink a m b -> m b
rsrc $$+- sink = do
    (ResumablePipe _ final, res) <- runPipe $ pipeResume rsrc sink
    final
    return res
{-# INLINE ($$+-) #-}

-- | Left fuse, combining a source and a conduit together into a new source.
--
-- Both the @Source@ and @Conduit@ will be closed when the newly-created
-- @Source@ is closed.
--
-- Leftover data from the @Conduit@ will be discarded.
--
-- Since 0.4.0
($=) :: Monad m => Source m a -> Conduit a m b -> Source m b
($=) = pipe
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
(=$) = pipe
{-# INLINE (=$) #-}

-- | Fusion operator, combining two @Pipe@s together into a new @Pipe@.
--
-- Both @Pipe@s will be closed when the newly-created @Pipe@ is closed.
--
-- Leftover data returned from the right @Pipe@ will be discarded.
--
-- Note: in previous versions, this operator would only fuse together two
-- @Conduit@s (known as middle fusion). This operator is generalized to work on
-- all @Pipe@s, including @Source@s and @Sink@s.
--
-- Since 0.4.0
(=$=) :: Monad m => Pipe a b r0 m r1 -> Pipe b c r1 m r2 -> Pipe a c r0 m r2
(=$=) = pipe
{-# INLINE (=$=) #-}

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
