Note: This issue has been resolved:

* No more chunking.
* isolate cannot force consumption.

Thinking out loud: my current problem with the Conduit type
===========================

The main issue with implementing a Conduit (i.e., the pipe between a source and
sink, not the overall package) is that of data loss. For example, take the
following code:

```haskell
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)

main = C.runResourceT $ CL.sourceList [1..10] C.$$ do
    strings <- CL.map show C.=$ CL.take 5
    liftIO $ putStr $ unlines strings
    rest <- CL.fold (+) 0
    liftIO $ putStrLn $ "Sum is: " ++ show rest
```

You would expect it to print the numbers 1 to 5 on individual lines, and then
print the sum of the numbers 6 to 10. Instead, it prints that the sum is "0".
This is because the `map` conduit has already converted the integers to
strings, and those values are now lost to the system.

However, the problem is worse than this, because the output will change based
on the chunking behavior. For example, the following code (perhaps correctly)
gives the sum as 40:

```haskell
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mappend)

main = C.runResourceT $ (CL.sourceList [1..5] `mappend` CL.sourceList [6..10]) C.$$ do
    strings <- CL.map show C.=$ CL.take 5
    liftIO $ putStr $ unlines strings
    rest <- CL.fold (+) 0
    liftIO $ putStrLn $ "Sum is: " ++ show rest
```

For the record, I believe that enumerator suffers from the same problem.
There's another problem in conduit: isolate currently only does half of its
job. We want `isolate` to both prevent a sink from consuming too much data, and
force the sink to consume all the data directed at it. In other words, the
output from the following program should be the numbers 11 to 15:

```haskell
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mappend)

main = C.runResourceT $ CL.sourceList [1..15] C.$$ do
    _ <- CL.isolate 5 C.=$ CL.consume
    _ <- CL.isolate 5 C.=$ return ()
    rest <- CL.consume
    liftIO $ print rest
```

Instead, however, the second isolate call consumes no data, and the result is
the numbers 6 to 15.

__Update__: I've decided that the issue of isolate not consuming all data is
not a real issue. Instead, I'm adding sinkNull to help out with forcing
consumption.

Possible solutions
---------------------------

One simple solution (on branch isolate-fix-1) to both problems is to set up the
fuse/connect operators (actually, I think just `=$`) to always pipe all data
into a conduit until it returns leftovers. This solves the ambiguity issue: we
will consistently return the same results despite chunking. And it also means
that isolate works. There are two downsides:

1. This can be very inefficient, forcing consumption of an entire source even
   if its not needed.

2. We're defaulting to throwing away data.

You can argue that (2) is acceptable, and if someone wants to preserve data,
they can either write a specialized conduit that can (somehow, I'm not quite
sure how) return leftovers early, or use isolate to make sure only some data is
consumed.

Another solution I'm thinking of, which I have no yet solidified, is changing
the definition of conduit somehow. Here are a bunch of ideas I have:

* We have the concept of closing from the left (i.e., the source is closed),
  where no leftover values need to be returned, versus closed from the right
  (i.e., the sink is closed), where no output values are produced.

* Conduits are required to return data piecemeal, i.e., no chunking. As soon as
  the consuming sink finishes processing, the conduit is notified before it
  processes any more content. This is in effect the opposite of
  `isolate-fix-1`. (It also doesn't solve the isolate issue.)

* When closing a conduit (from the right using the terminology above), the
  conduit can provide a new sink which will consume as much input as it wants
  and then provide leftovers.
