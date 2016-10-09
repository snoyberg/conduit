Conduit is a framework for dealing with streaming data, such as
reading raw bytes from a file, parsing a CSV response body from an
HTTP request, or performing an action on all files in a directory
tree. It standardizes various interfaces for streams of data, and
allows a consistent interface for transforming, manipulating, and
consuming that data.

Some of the reasons you'd like to use conduit are:

* Constant memory usage over large data
* Deterministic resource usage (e.g., promptly close file handles)
* Easily combine different data sources (HTTP, files) with data
  consumers (XML/CSV processors)

Want more motivation on why to use conduit? Check out
[this presentation on conduit](https://docs.google.com/presentation/d/1RBefOCZ7AKOo4f1yiF4mtKPAT3l5vY9ky2SR02O4Vvg/edit?usp=sharing).

## Synopsis

Basic examples of conduit usage, much more to follow!

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
import Conduit

main = do
    -- Pure operations: summing numbers.
    print $ runConduitPure $ yieldMany [1..10] .| sumC

    -- Exception safe file access: copy a file.
    writeFile "input.txt" "This is a test." -- create the source file
    runConduitRes $ sourceFileBS "input.txt" .| sinkFile "output.txt" -- actual copying
    readFile "output.txt" >>= putStrLn -- prove that it worked

    -- Perform transformations.
    print $ runConduitPure $ yieldMany [1..10] .| mapC (+ 1) .| sinkList
```

## Libraries

There are a large number of packages relevant to conduit, just search
for conduit on
[the LTS Haskell package list page](https://www.stackage.org/lts).  In
this tutorial, we're going to rely mainly on the
[conduit-combinators](https://www.stackage.org/package/conduit-combinators)
library, which provides a large number of common functions
built-in. If you're looking for something lighter-weight, you can use
the [conduit](https://www.stackage.org/package/conduit) library, which
defines the core datatypes and primitive functions, and
[conduit-extra](https://www.stackage.org/package/conduit-extra), which
adds support for many common low-level operations

Generally, you should use conduit-combinators unless you're an open
source library author looking to reduce your transitive dependency
footprint.

This tutorial relies on conduit-combinators version 1.0.8 or
later. The examples are
[Stack scripts](https://haskell-lang.org/tutorial/stack-script).

## Conduit as a bad list

Let's start off by comparing conduit to normal lists. We'll be able to
compare and contrast with functions you're already used to working
with.

``` haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = do
    putStrLn "List version:"
    print $ take 10 [1..]
    putStrLn ""
    putStrLn "Conduit version:"
    print $ runConduitPure $ yieldMany [1..] .| takeC 10 .| sinkList
```

Our list function is pretty straightforward: creating an infinite list
from 1 and ascending, take the first 10 elements, and then print the
list. The conduit version does the exact same thing, but:

* In order to convert the `[1..]` list into a conduit, we use the
  `yieldMany` function. (And note that, like lists, conduit has no
  problem dealing with infinite streams.)
* We're not just doing function composition, and therefore we need to
  use the `.|` composition operator. This combines multiple components
  of a conduit pipeline together.
* Instead of `take`, we use `takeC`. The `Conduit` module provides
  many functions matching common list functions, but appends a `C` to
  disambguate the names. (If you'd prefer to use a qualified import,
  check out
  [Data.Conduit.Combinators](https://www.stackage.org/haddock/lts-6.19/conduit-combinators-1.0.8/Data-Conduit-Combinators.html)).
* To consume all of our results back into a list, we use `sinkList`
* We need to explicitly run our conduit pipeline to get a result from
  it. Since we're running a pure pipeline (no monadic effects), we can
  use `runConduitPure`.
* And finally, the data flows from left to right in the conduit
  composition, as opposed to right to left in normal function
  composition. There's nothing deep to this; it's just intended to
  make conduit feel more like common streaming abstraction from other
  places. For example, notice how similar the code above looks to
  piping in a Unix shell: `ps | grep ghc | wc -l`.

Alright, so what we've established is that we can use conduit as a
bad, inconvenient version of lists. Don't worry, we'll soon start to
see cases where conduit far outshines lists, but we're not quite there
yet. Let's build up a slightly more complex pipeline:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = do
    putStrLn "List version:"
    print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
    putStrLn ""
    putStrLn "Conduit version:"
    print $ runConduitPure
          $ yieldMany [1..]
         .| takeC 10
         .| mapC (* 2)
         .| takeWhileC (< 18)
         .| sinkList
```

Nothing more magical going on, we're just looking at more
functions. For our last bad-list example, let's move over from a pure
pipeline to one which performs some side effects. Instead of
`print`ing the whole result list, let's use `mapM_C` to print each
value individually.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = do
    putStrLn "List version:"
    mapM_ print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
    putStrLn ""
    putStrLn "Conduit version:"
    runConduit
          $ yieldMany [1..]
         .| takeC 10
         .| mapC (* 2)
         .| takeWhileC (< 18)
         .| mapM_C print
```

For the list version, all we've done is added `mapM_` at the
beginning. In the conduit version, we replace `print $ runConduitPure`
with `runConduit` (since we're no longer generating a result to print,
and our pipeline now has effects), and replaced `sinkList` with
`mapM_C print`. We're no longer reconstructing a list at the end,
instead just streaming the values one at a time into the `print`
function.

## Interleaved effects

Let's make things a bit more difficult for lists. We've played to
their strengths until now, having a pure series of functions composed,
and then only performing effects at the end (either `print` or `mapM_
print`). Suppose we have some new function:

```haskell
magic :: Int -> IO Int
magic x = do
    putStrLn $ "I'm doing magic with " ++ show x
    return $ x * 2
```

And we want to use this in place of the `map (* 2)` that we were doing
before. Let's see how the list and conduit versions adapt:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

magic :: Int -> IO Int
magic x = do
    putStrLn $ "I'm doing magic with " ++ show x
    return $ x * 2

main :: IO ()
main = do
    putStrLn "List version:"
    mapM magic (take 10 [1..]) >>= mapM_ print . takeWhile (< 18)
    putStrLn ""
    putStrLn "Conduit version:"
    runConduit
          $ yieldMany [1..]
         .| takeC 10
         .| mapMC magic
         .| takeWhileC (< 18)
         .| mapM_C print
```

Notice how different the list version looks: we needed to break out
`>>=` to allow us to have two different side-effecting actions (`mapM
magic` and `mapM_ print`). Meanwhile, in conduit, all we did was
replace `mapC (* 2)` with `mapMC magic`. This is where we begin to see
the strength of conduit: it allows us to build up large pipelines of
components, and each of those components can be side-effecting!

However, we're not done with the difference yet. Try to guess what the
output will be, and then ideally run it on your machine and see if
you're correct. For those who won't be running it, here's the output:

```
List version:
I'm doing magic with 1
I'm doing magic with 2
I'm doing magic with 3
I'm doing magic with 4
I'm doing magic with 5
I'm doing magic with 6
I'm doing magic with 7
I'm doing magic with 8
I'm doing magic with 9
I'm doing magic with 10
2
4
6
8
10
12
14
16

Conduit version:
I'm doing magic with 1
2
I'm doing magic with 2
4
I'm doing magic with 3
6
I'm doing magic with 4
8
I'm doing magic with 5
10
I'm doing magic with 6
12
I'm doing magic with 7
14
I'm doing magic with 8
16
I'm doing magic with 9
```

In the list version, we apply the `magic` function to all 10 elements
in the initial list, printing all the output at once and generating a
new list. We then use `takeWhile` on this new list and exclude the
values 18 and 20. Finally, we print out each element in our new
8-value list. This has a number of downsides:

* We had to force all 10 items of the list into memory at once. For 10
  items, not a big deal. But if we were dealing with massive amounts
  of data, this could cripple our program.
* We did "more magic" than was strictly necessary: we applied `magic`
  to 10 items in the list. However, our `takeWhile` knew when it
  looked at the 9th result that it was going to ignore the rest of the
  list. Nonetheless, because our two components (`magic` and
  `takeWhile`) are separate from each other, we couldn't know that.

Let's compare that to the conduit version:

* From the output, we can see that the calls to `magic` are
  interleaved with the calls to `print`. This shows that our data
  flows through the whole pipeline one element at a time, and never
  needs to build up an intermediate list. In other words, we get
  constant memory usage in this pipeline, a huge selling point for
  conduit.
* Notice that we only perform "magic" 9 times: once we run `magic` on
  9, get a result of 18, and find out that it fails our `takeWhileC (<
  18)`, the conduit pipeline doesn't demand any more values, and
  therefore `magic` isn't run again. We'll describe in more detail
  later how conduit is consumer-driven, but this is your first taste
  of this.

To be clear, it's entirely possible to get this behavior with a
list-based program. What you'll lose is easy composition. For example,
here's one way to get the same behavior as was achieved with conduit:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

magic :: Int -> IO Int
magic x = do
    putStrLn $ "I'm doing magic with " ++ show x
    return $ x * 2

main :: IO ()
main = do
    let go [] = return ()
        go (x:xs) = do
            y <- magic x
            if y < 18
                then do
                    print y
                    go xs
                else return ()

    go $ take 10 [1..]
```

Notice how we've had to reimplement the behavior of `takeWhile`,
`mapM`, and `mapM_` ourselves, and our logic is much harder to
follow. Conduit makes it easy to get the right behavior: interleaved
effects, constant memory, and (as we'll see later) deterministic
resource usage.

## Terminology and concepts

Let's take a step back from the code and discuss some terminology and
concepts in conduit. Conduit deals with _streams_ of data. Each
_component_ of a _pipeline_ can _consume_ data from _upstream_, and
_produce_ data to send _downstream_. For example:

```haskell
runConduit $ yieldMany [1..10] .| mapC show .| mapM_C print
```

In this snippet, `yieldMany [1..10]`, `mapC show`, and `mapM_C print` are
each components. We use the `.|` operator - a synonym for the
[`fuse` function](https://www.stackage.org/haddock/lts-6.19/conduit-1.2.8/Data-Conduit.html#v:fuse) -
to compose these components into a pipeline. Then we run that pipeline
with `runConduit`.

From the perspective of `mapC show`, `yieldMany [1..10]` is its
upstream, and `mapM_C` is its downstream. When we look at `yieldMany
[1..10] .| mapC show`, what we're actually doing is combining these
two components into a larger component. Let's look at the streams
involved:

* `yieldMany` consumes nothing from upstream, and produces a stream of
  `Int`s
* `mapC show` consumes a stream of `Int`s, and produces a stream of
  `String`s
* When we combine these two components together, we get something
  which consumes nothing from upstream, and produces a stream of
  `String`s.

To add some type signatures into this:

```haskell
yieldMany [1..10] :: ConduitM ()  Int    IO ()
mapC show         :: ConduitM Int String IO ()
```

There are four type parameters to `ConduitM`

* The first indicates the upstream value, or input. For `yieldMany`,
  we're using `()`, though really it could be any type since we never
  read anything from upstream. For `mapC`, it's `Int`
* The second indicates the downstream value, or output. For
  `yieldMany`, this is `Int`. Notice how this matches the input of
  `mapC`, which is what lets us combine these two. The output of
  `mapC` is `String`.
* The third indicates the base monad, which tells us what kinds of
  effects we can perform. A `ConduitM` is a monad transformer, so you
  can use `lift` to perform effects. (We'll learn more about conduit's
  monadic nature later.) We're using `IO` in our example.
* The final indicates the result type of the component. This is
  typically only used for the most downstream component in a
  pipeline. We'll get into this when we discuss folds below.

Let's also look at the type of our `.|` operator:

```haskell
(.|) :: Monad m
     => ConduitM a b m ()
     -> ConduitM b c m r
     -> ConduitM a c m r
```

This shows us that:

* The output from the first component much match the input from the
  second
* We ignore the result type from the first component, and keep the
  result of the second
* The combined component consumes the same type as the first component
  and produces the same type as the second component
* Everything has to run in the same base monad

__Exercise__ Work through what happens when we add `.| mapM_C print`
to the mix above.

Finally, let's look at the type of the `runConduit` function:

```haskell
runConduit :: Monad m => ConduitM () Void m r -> m r
```

This gives us a better idea of what a pipeline is: just a self
contained component, which consumes nothing from upstream (denoted by
`()`) and producing nothing to downstream (denoted by `Void`)\*. When
we have such a stand-alone component, we can run it to extract a
monadic action that will return a result (the `m r`).

\* The choice of `()` and `Void` instead of, say, both `()` or both
`Void`, is complicated. For now, I recommend just accepting that this
makes sense. The short explanation is that the input is in negative
position whereas the output is in positive position, and therefore we
can give the stronger `Void` guarantee in the output case.

Finally, we talked about pure pipelines before. Those are just
pipelines with `Identity` as the base monad:

```haskell
runConduitPure :: ConduitM () Void Identity r -> r
```

## Folds

A common activity with lists is folding down to a single result. This
concept translates directly into conduit, and works nicely at ensuring
constant memory usage. If you're familiar with folding over lists, the
concepts here should be pretty straightforward, so this will mostly
just be a collection of examples.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..100 :: Int] .| sumC
```

Summing is straightforward, and can be done if desired with the
`foldlC` function:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..100 :: Int] .| foldlC (+) 0
```

You can use `foldMapC` to fold monoids together:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit
import Data.Monoid (Sum (..))

main :: IO ()
main = print $ getSum $ runConduitPure $ yieldMany [1..100 :: Int] .| foldMapC Sum
```

Or you can use `foldC` as a shortened form of `foldMapC id`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = putStrLn $ runConduitPure
     $ yieldMany [1..10 :: Int]
    .| mapC (\i -> show i ++ "\n")
    .| foldC
```

Though if you want to make that easier you can use `unlinesC`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = putStrLn $ runConduitPure
     $ yieldMany [1..10 :: Int]
    .| mapC show
    .| unlinesC
    .| foldC
```

You can also do monadic folds:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit
import Data.Monoid (Product (..))

magic :: Int -> IO (Product Int)
magic i = do
    putStrLn $ "Doing magic on " ++ show i
    return $ Product i

main :: IO ()
main = do
    Product res <- runConduit $ yieldMany [1..10] .| foldMapMC magic
    print res
```

Or with `foldMC`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit
import Data.Monoid (Product (..))

magic :: Int -> Int -> IO Int
magic total i = do
    putStrLn $ "Doing magic on " ++ show i
    return $! total * i

main :: IO ()
main = do
    res <- runConduit $ yieldMany [1..10] .| foldMC magic 1
    print res
```

There are plenty of other functions available in the
conduit-combinator library. We won't be covering all of them in this
tutorial, but hopefully this crash-course will give you an idea of
what kinds of things you can do and help you understand the API docs.

* * *

# FIXME NEED TO EDIT BELOW HERE

* Transformations
  * filterCE
  * mapC, mapMC
  * concatC
* Monadic composition
  * In a source
  * In a consumer
* Primitives
  * await
  * yield
  * leftover
  * demonstrate taking a single byte from a ByteString
* Monadic effects
* Type synonyms
* Driven by downstream
* Chunked data
* Leftovers
* ResourceT
* ZipSink
  * Average
  * Save a file and get its hash
* ZipSource
* ZipConduit
* Fuse with leftovers/upstream results
* Stream-of-streams, `lineC`, `takeExactlyC`

## Features of conduit

* `conduit` allows you to deal with large- and possibly infinite- streams of data in constant memory. Chunks of data are dealt with one piece at a time instead of needing to read in the entire body at once.
* `conduit` also allows for deterministic resource usage. When using scarce resources like file descriptors, `conduit` is designed to immediately recycle resources when they are no longer needed. Contrast this with lazy I/O, which provides for constant memory usage, but at the expense of deterministic resource usage.
* Resource usage must also be exception safe: a file handle must be recycled even in the presence of exceptions. Most of this functionality is provided by the associated `resourcet` package, described below.
* It should be easy to compose together pre-built components to build up more complex structures. Our goal is to retain the composability of pure code while dealing with the imperative world.

## Basics

The main module for the `conduit` package is Data.Conduit, which provides the core data types and primitive operations. Another commonly used modules is Data.Conduit.List, which provides a number of helper functions for common cases, based on standard Haskell concepts like `map` and `fold`.

There are three main concepts in conduit. A `Source` will produce a stream of data values and send them *downstream*. A `Sink` will consume a stream of data values from *upstream* and produce a return value. In the first example in the synopsis, `sourceList` generated a stream of `Integer`s which the `fold` consumed, producing a return value of 55. The third concept is the `Conduit`, which consumes a stream of values from upstream and produces a new stream to send downstream. In the synopsis, the call to `map` consumed one stream of `Integer`s, added 1 to each value, and sent the new results downstream.

In order to combine these different components, we have *connecting* and *fusing*. The connect operator is `$$`, and it will combine a `Source` and `Sink`, feeding the values produced by the former into the latter, and producing a final result. Fusion, on the other hand, will take two components and generate a new component. For example, `=$` can fuse a `Conduit` and `Sink` together into a new `Sink`, which will consume the same values as the original `Conduit` and produce the same result as the original `Sink`. The other two fusion operators are `$=`, which combines a `Source` and `Conduit` into a new `Source`, and `=$=`, which combines two `Conduit`s into a new `Conduit`.

```haskell
-- Demonstration of connect and fuse operators
import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Source IO Int -- produces a stream of Ints
source = CL.sourceList [1..4]

sink :: Sink String IO () -- consumes a stream of Strings, no result
sink = CL.mapM_ putStrLn

conduit :: Conduit Int IO String -- converts Ints into Strings
conduit = CL.map show

main = do
    source $$ conduit =$ sink
    -- alternatively, with the same meaning
    source $= conduit $$ sink
```

__Exercise__: Write a `Conduit` which will multiply all incoming numbers by 2, and then include it in the above code snippet. Note that there are multiple ways of using the connect and fusion operators to get the desired result, give a few of them a shot.

* * *

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Source IO Int -- produces a stream of Ints
source = CL.sourceList [1..4]

sink :: Sink String IO () -- consumes a stream of Strings, no result
sink = CL.mapM_ putStrLn

conduit :: Conduit Int IO String -- converts Ints into Strings
conduit = CL.map show

newConduit :: Conduit Int IO Int
newConduit = CL.map (* 2)

main = do
    -- each of the following does the same thing
    source $$ newConduit =$ conduit =$ sink
    source $= newConduit $= conduit $$ sink
    source $= newConduit $$ conduit =$ sink
    source $$ (newConduit =$= conduit) =$ sink
    source $= (newConduit =$= conduit) $$ sink
```

### Unified data type

Under the surface, all three core data types are just wrappers around the same type, `ConduitM`. By wrapping around this single unified type, we're able to reuse a lot of code and more easily compose components in conduit.

`ConduitM` takes four type parameters: input received from upstream, output send downstream, the underlying monad, and the return value. Our specialized types are defined as:

```haskell
type Source m o = ConduitM () o m () -- no meaningful input or return value
type Conduit i m o = ConduitM i o m () -- no meaningful return value
type Sink i m r = ConduitM i Void m r -- no meaningful output value
```

`ConduitM` is a monad transformer. As such, you can lift operations from the underlying monad (see "Lifting Operations" below), and can easily compose together multiple components. This makes it simple to build up complex mechanisms from simpler components.

You will very rarely need to interface directly with the `ConduitM` datatype, though it will occasionally appear in error messages.

## Primitives

There are three core primitives in the `conduit` library.

1. `await` will take a single value from upstream, if available.
2. `yield` will send a single value downstream.
3. `leftover` will put a single value back in the upstream queue, ready to be read by the next call to `await`.

```haskell
-- Using primitives
import Data.Conduit
import Control.Monad.IO.Class

source :: Source IO Int
source = do
    yield 1
    yield 2
    yield 3
    yield 4
    
conduit :: Conduit Int IO String
conduit = do
    -- Get all of the adjacent pairs from the stream
    mi1 <- await
    mi2 <- await
    case (mi1, mi2) of
        (Just i1, Just i2) -> do
            yield $ show (i1, i2)
            leftover i2
            conduit
        _ -> return ()
            
sink :: Sink String IO ()
sink = do
    mstr <- await
    case mstr of
        Nothing -> return ()
        Just str -> do
            liftIO $ putStrLn str
            sink
            
main = source $$ conduit =$ sink
```

### Exercises

1.  Implement `sourceList` in terms of `yield`.

    ```haskell
    import Data.Conduit
    import qualified Data.Conduit.List as CL
    
    sourceList :: Monad m => [o] -> Source m o
    sourceList = ???
    
    main = sourceList [1, 2, 3] $$ CL.mapM_ print
    ```

    * * *

    ```haskell
    sourceList :: Monad m => [o] -> Source m o
    sourceList = mapM_ yield
    ```

2.  There's a helper function in the library called awaitForever. Rewrite `sink` above using `awaitForever`.

    ```haskell
    import Data.Conduit
    import Control.Monad.IO.Class
    
    source :: Source IO Int
    source = do
        yield 1
        yield 2
        yield 3
        yield 4
        
    conduit :: Conduit Int IO String
    conduit = do
        -- Get all of the adjacent pairs from the stream
        mi1 <- await
        mi2 <- await
        case (mi1, mi2) of
            (Just i1, Just i2) -> do
                yield $ show (i1, i2)
                leftover i2
                conduit
            _ -> return ()


    sink :: Sink String IO ()
    sink = ???
            
    main = source $$ conduit =$ sink
    ```

    * * *
    
    ```haskell
    sink :: Sink String IO ()
    sink = awaitForever $ liftIO . putStrLn
    ```

3.  Implement your own version of `awaitForever`.

    ```haskell
    import Data.Conduit
    import qualified Data.Conduit.List as CL
    import Control.Monad.Trans.Class (lift)
    
    myAwaitForever :: Monad m => (i -> Conduit i m o) -> Conduit i m o
    myAwaitForever f = ???
    
    main = CL.sourceList [1..10] $$ myAwaitForever (lift . print)
    ```

    * * *
    
    ```haskell
    myAwaitForever :: Monad m => (i -> Conduit i m o) -> Conduit i m o
    myAwaitForever f = 
        await >>= maybe (return ()) (\x -> f x >> myAwaitForever f)
    ```

## Monadic chaining

The above examples demonstrated how we can combine primitives together using standard monadic binding. This doesn't just apply to the primitives: you can combine larger components together as well. Consider the "triple" `Conduit` which will output any values it receives three times:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

triple :: Monad m => Conduit a m a
-- Triple conduit
triple = do
    ma <- await
    case ma of
        Nothing -> return ()
        Just a -> do
            CL.sourceList [a, a, a]
            triple

main = CL.sourceList [1..4] $$ triple =$ CL.mapM_ print
```

Mini exercise: rewrite the above using `awaitForever`, which should be much shorter.

As you can see, it's entirely possible to combine a higher-level function like `sourceList` into a larger function. One question you might have is: how can we use a `Source` inside the body of a `Conduit`? We'll discuss that with `Producer`s and `Consumer`s in the generalizing section below.

__Exercise__: Write a `Conduit` that consumes a stream of `Int`s. It takes the first `Int` from the stream, and then multiplies all subsequent `Int`s by that number and sends them back downstream. You should use the Data.Conduit.List.map function for this.

* * *

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

-- Multiplier
multiplier = do
    ma <- await
    case ma of
        Nothing -> return ()
        Just a -> CL.map (* a)

main = CL.sourceList [5..10] $$ multiplier =$ CL.mapM_ print
```

## Lifting operations

As monad transformers, our components can perform any operations supported by their underlying monads. We've already seen this implicitly with usage of `CL.mapM_ print`. However, we can also use `lift` or `liftIO` explicitly. And we're also not limited to the `IO` monad. Let's consider an example with the `State` monad.

```haskell
import Control.Monad.State
import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Source (State Int) Int
source = do
    x <- lift get
    if x <= 0
        then return ()
        else do
            yield x
            lift $ modify (\x -> x - 2)
            source
            
conduit :: Conduit Int (State Int) (Int, Int)
conduit = awaitForever $ \i -> do
    lift $ modify (+ 1)
    x <- lift get
    yield (i, x)

main :: IO ()
main = do
    let result :: State Int [(Int, Int)]
        result = source $$ conduit =$ CL.consume
    print $ runState result 5
```

## Generalizing

In the triple conduit example above, why were we able to use a `Source` inside a `Conduit`? That shouldn't typecheck, should it? After all, a `Source` has its input type constrained to `()`, while a `Conduit` can have any input type. In our example above, the input was `Int`, so it certainly should not have worked.

The answer is that we have two final type synonyms to introduce. `Producer` is a generalized `Source`. Instead of stating that it consumes an input stream of `()` values, it can consume *any* input values, thus allowing it to work for both `Source` and `Conduit`. Similarly, we have `Consumer` which can output any values, and thus works as either a `Conduit` or a `Sink`.

In the interest of generality, most library functions will be written in terms of `Producer` or `Consumer`. As a user, you can simply use `Source` and `Sink` in most of your code, unless you need to use a function as `Conduit` as well.

And for the cases where you have a `Source` and wish to convert it to a `Conduit` after the fact (e.g., it comes from another person's library), you can use toProducer, or toConsumer for `Sink`s.

## Termination

Let's talk about the lifetime of a sequence of components (we'll call it a pipeline). A pipeline is always driven from *downstream*. This means, for example, that if we connect a `Source` and a `Sink`, we start our execution with the `Sink`.

The Sink will continue processing- however that specific Sink processes- until it needs more input. Then it calls `await`, and processing is paused until new input is available. For the Source, it will be woken up when the downstream component asks for input, and will yield control downstream when it yields a value. As soon as the `Sink` completes, the entire pipeline terminates.

The following example demonstrates how the components of our pipeline interact with each other. Try modifying the parameter to `sink` from 2 to 4 to see how that affects the output.

```haskell
import Data.Conduit
import Control.Monad.IO.Class

-- Termination

source = do
    liftIO $ putStrLn "source: yielding 1"
    yield 1
    liftIO $ putStrLn "source: yielding 2"
    yield 2
    liftIO $ putStrLn "source: yielding 3"
    yield 3
    
conduit = do
    liftIO $ putStrLn "conduit calling await"
    mx <- await
    case mx of
        Nothing -> liftIO $ putStrLn "Nothing left, exiting"
        Just x -> do
            liftIO $ putStrLn $ "conduit yielding " ++ show x
            yield x
            conduit
            
sink 0 = liftIO $ putStrLn "sink is finished, terminating"
sink i = do
    liftIO $ putStrLn $ "sink: still waiting for " ++ show i
    mx <- await
    case mx of
        Nothing -> liftIO $ putStrLn "sink: Nothing from upstream, exiting"
        Just x -> do
            liftIO $ putStrLn $ "sink received: " ++ show x
            sink (i - 1)
            
main = source $$ conduit =$ sink 2
```

Based on what we've said until now, there's a big limitation. `Source`s and `Conduit`s have no way of cleaning up after themselves, since they are terminated immediately when anything downstream from them terminates. To address this, `conduit` supports the concept of terminators. Each time a `Source` or `Conduit` `yield`s a value downstream, it may additionally include a clean-up function. Each time the `Source` `yield`s, it overwrites the previously yielded clean-up function. Let's see a simple example:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

source =
    loop 1
  where
    loop i = do
        yieldOr i $ putStrLn $ "Terminated when yielding: " ++ show i
        loop $ i + 1
        
main = source $$ CL.isolate 7 =$ CL.mapM_ print
```

While in our examples till now this isn't incredibly important, when we start dealing with scarce resources like file descriptors, we need the ability to close the descriptor as soon as we're done with it.

Manually inserting `yieldOr`s throughout your codebase can be very tedious. Instead, it's usually easier to use the `addCleanup` function, which will ensure that a certain function is called on termination. Your cleanup function is provided a `Bool` parameter. If `True`, it means that the component ran to its normal completion. If `False`, it means that downstream terminated first.

Let's demonstrate some simple file I/O. Note that the code below deals with characters one at a time, and is thus incredibly inefficient. It is highly recommended to use Data.Conduit.Binary for real-world use cases.

```haskell
{-# START_FILE test.txt #-}
This is a test.
{-# START_FILE main.hs #-}
import System.IO
import Data.Conduit
import Control.Monad.IO.Class
import qualified Data.Conduit.List as CL

source = do
    handle <- liftIO $ openFile "test.txt" ReadMode
    addCleanup (const $ putStrLn "Closing handle" >> hClose handle) $ loop handle
  where
    loop handle = do
        eof <- liftIO $ hIsEOF handle
        if eof
            then return ()
            else do
                c <- liftIO $ hGetChar handle
                yield c
                loop handle
                
main = source $$ CL.isolate 5 =$ CL.mapM_ print
```

## Exception Safety

There's still a major flaw in our `addCleanup` approach: it's not exception safe! If an exception is thrown by either our component or any other component in the pipeline, our `Handle` will not be closed correctly.

In order to guarantee that an action takes place even in the presence of exceptions, we need to introduce one final function: `bracketP`. It works very similarly to the standard `bracket` function: you provide it an allocate function which creates some scarce resource, a cleanup function which frees that resource, and an inside function which will perform some action with that resource.

Let's rewrite our inefficient file reader above to use `bracketP`.

```haskell
{-# START_FILE test.txt #-}
This is a test.
{-# START_FILE main.hs #-}
import System.IO
import Data.Conduit
import Control.Monad.IO.Class
import qualified Data.Conduit.List as CL

source =
-- Better with bracketP
    bracketP
        (openFile "test.txt" ReadMode)
        (\handle -> putStrLn "Closing handle" >> hClose handle)
        loop

  where
    loop handle = do
        eof <- liftIO $ hIsEOF handle
        if eof
            then return ()
            else do
                c <- liftIO $ hGetChar handle
                yield c
                loop handle

-- An exception-throwing sink.
exceptionalSink = do
    c <- await
    liftIO $ print c
    error "This throws an exception"

-- We also need to call runResourceT
main = runResourceT $ source $$ exceptionalSink
```

Notice our call to `runResourceT`. At the point where execution leaves that function, all resources allocated inside that block will be freed. For more information on `ResourceT`, please see Control.Monad.Trans.Resource.

## Connect and resume

`conduit` introduces a form of inversion of control. You no longer control the flow of execution of your program. Instead, you declaratively state when individual components need input and provide output, and then `conduit` ensures that everything is passed around correctly. For many use cases, this is sufficient. However, there are some cases where you may want to have more control over the flow of execution. Connect and resume provides you such an escape route.

Connect and resume introduces the concept of a __resumable source__. This is a `Source` which has been partially run, but can be continued by reconnecting to another `Sink`. To create a `ResumableSource`, you use the `$$+` connect-and-resume operator. To reconnect a `ResumableSource` to a new `Sink` and get an updated `ResumableSource`, use the `$$++` operator. And finally, you use the `$$+-` to connect a `ResumableSource` to its final `Sink`.

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

main = do
    (rsrc1, result1) <- CL.sourceList [1..10] $$+ CL.take 3
    (rsrc2, result2) <- rsrc1 $$++ CL.take 3
    result3 <- rsrc2 $$+- CL.consume
    print (result1, result2, result3)
```

The important thing to note about this `ResumableSource` is that it might have some cleanup function associated with it, so you *must* ultimately call `$$+-` or else risk delaying cleanup of those resources.

Connect and resume usually only comes up in more complicated control flow operations, so it's likely that you won't run into it in your normal usage of `conduit`. One library which does utilize this is http-conduit, where a `ResumableSource` is returned from the http function to represent the body of an HTTP response.

## Further reading

* Data.Conduit- Main module defining data types and core operations.
* Data.Conduit.List- A collection of common helper utilities.
* Data.Conduit.Network- Create network servers and clients.


Below is a tutorial, originally posted on School of Haskell, on how to use conduit. In addition to this tutorial, there is [a set of slides on conduit](https://docs.google.com/presentation/d/1RBefOCZ7AKOo4f1yiF4mtKPAT3l5vY9ky2SR02O4Vvg/edit?usp=sharing) which covers a number of topics.
http://www.yesodweb.com/blog/2014/03/network-conduit-async
https://www.fpcomplete.com/blog/2016/09/practical-haskell-simple-file-mirror-1
https://www.fpcomplete.com/blog/2016/09/practical-haskell-simple-file-mirror-2
