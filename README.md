`conduit` is a solution to the streaming data problem, allowing for production, transformation, and consumption of streams of data in constant memory. It can be used for processing files, dealing with network interfaces, or parsing structured data in an event-driven manner.

Below is a tutorial, originally posted on School of Haskell, on how to use conduit. In addition to this tutorial, there is [a set of slides on conduit](https://docs.google.com/presentation/d/1RBefOCZ7AKOo4f1yiF4mtKPAT3l5vY9ky2SR02O4Vvg/edit?usp=sharing) which covers a number of topics.

## Libraries

There are a large number of packages relevant to conduit, just search for
conduit on [the LTS Haskell package list page](https://www.stackage.org/lts).
However, there are three core packages most users will be interested in:

* [conduit](https://www.stackage.org/package/conduit) defines the core data types, functions, and abstractions for using conduit, and provides the `Data.Conduit.List` module, containing some of the most common streaming functions. It has minimal dependencies.
* [conduit-extra](https://www.stackage.org/package/conduit-extra) adds support for many common low-level operations, like streaming from files, performing textual encodings/decodings, etc. It adds more dependencies than the conduit package itself, but these are the libraries that most applications will end up depending on anyway.
* [conduit-combinators](https://www.stackage.org/package/conduit-combinators) is the fully-loaded conduit library, offering a complete set of combinator functions and abstractions over chunked data (like `ByteString`, `Text`, and `Vector`). Importing the `Conduit` module will get you completely up-and-running with the conduit toolchain. The downside is that there are a significant number of dependencies.

If you're looking for advice: use `conduit-combinators` if you're writing an
application. If you're writing a library where others will be concerned about
the transitive dependency list, try sticking to `conduit` and `conduit-extra`
to start off.

## Synopsis

```haskell
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

main = do
    -- Pure operations: summing numbers.
    result <- CL.sourceList [1..10] $$ CL.fold (+) 0
    print result
    
    -- Exception safe file access: copy a file.
    writeFile "input.txt" "This is a test."
    runResourceT $ CB.sourceFile "input.txt" $$ CB.sinkFile "output.txt"
    readFile "output.txt" >>= putStrLn
    
    -- Perform transformations.
    result <- CL.sourceList [1..10] $$ CL.map (+ 1) =$ CL.consume
    print result
```

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
