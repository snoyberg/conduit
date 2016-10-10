## Introduction

Whenever you run an external process, there are four ways\* to interact with it post-creation:

* Write to its standard input
* Read from its standard output
* Read from its standard error
* Check its exit code

The standard `System.Process` module provides means for all of these interactions. However, there are some downsides with using them:

* Many of the function in System.Process rely on lazy I/O.
* There is a subtle race condition when checking for exit codes.
* Dealing with `Handle`s directly is relatively low-level.

Data.Conduit.Process provides a higher-level interface for these four interactions, based on conduit. It additionally leverages type classes to provide more static type safety than dealing directly with System.Process, as will be described below. The library is also designed to work with the wonderful async library, providing for easy, high-quality concurrency.

Some important headlines before we begin:

* Providing general parameters for creating a process, such as its working directory or modified environment variables, are not addressed by this module; you should instead use the standard facilities from System.Process.
* When using this module, you should generally use the multithreaded runtime (`-threaded`). The underlying `waitForProcess` function will block all threads on the singlethreaded runtime, which can be problematic.
* \*When I said there are four ways to interact, that refers to the primary System.Process API. See the section below on pipes and signals for more information.

## Synopsis

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (await, yield, ($$), (=$))
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (ClosedStream (..), streamingProcess,
                                           proc, waitForStreamingProcess)
import           System.IO                (stdin)

main :: IO ()
main = do
    putStrLn "Enter lines of data. I'll base64-encode it."
    putStrLn "Enter \"quit\" to exit."

    ((toProcess, close), fromProcess, ClosedStream, cph) <-
        streamingProcess (proc "base64" [])

    let input = CB.sourceHandle stdin
             $$ CB.lines
             =$ inputLoop
             =$ toProcess

        inputLoop = do
            mbs <- await
            case mbs of
                Nothing -> close
                Just "quit" -> close
                Just bs -> do
                    yield bs
                    inputLoop

        output = fromProcess $$ CL.mapM_
            (\bs -> putStrLn $ "from process: " ++ show bs)

    ec <- runConcurrently $
        Concurrently input *>
        Concurrently output *>
        Concurrently (waitForStreamingProcess cph)

    putStrLn $ "Process exit code: " ++ show ec
```

## Exit codes

There's a [well documented corner case](https://ghc.haskell.org/trac/ghc/ticket/9292) in `waitForProcess` whereby multiple calls can end up in a race condition, and therefore a deadlock. Data.Conduit.Process works around this issue by not providing direct access to a `ProcessHandle`. Instead, it wraps this with a `StreamingProcessHandle`, which uses an STM TMVar under the surface. This allows you to either poll to check if a process has exited, or block and wait for the process to exit. As a minimal example (ignore the streaming bits for now, they'll be explained shortly):

```haskell
import Data.Conduit.Process

main :: IO ()
main = do
    (Inherited, Inherited, Inherited, sph) <-
        streamingProcess (shell "sleep 2")

    -- non-blocking
    getStreamingProcessExitCode sph >>= print

    -- blocking
    waitForStreamingProcess sph >>= print
```

If you need direct access to the ProcessHandle (e.g., to terminate a process), you can use `streamingProcessHandleRaw`.

## Streaming

Now to the main event: streaming data. There are multiple ways you can interact with streams with an external process:

* Let the child process inherit the stream from the parent process
* Provide a pre-existing `Handle`.
* Create a new `Handle` to allow more control of the interaction.

One downside of the System.Process API is that there is no static type safety to ensure that the `std_out` parameter in fact matches up with the value produced by `createProcess` for the standard output handle. To overcome this, Data.Conduit.Process makes use of type classes to represent the different ways to create a stream. This isn't entirely intuitive from the Haddocks, but once you see the concept used, it's easy to use yourself.

### Inherited and ClosedStream

Let's start with an example of using the simplest instances of our typeclasses. `Inherited` says to inherit the `Handle` from the parent process, while `ClosedStream` says to close the stream to the child process. For example, the next snippet will inherit stdin and stdout from the parent process and close standard error.

```haskell
import Data.Conduit.Process

main :: IO ()
main = do
    putStrLn "Just wrapping cat. Use Ctrl-D to exit."

    (Inherited, Inherited, ClosedStream, cph) <-
        streamingProcess (shell "cat")

    waitForStreamingProcess cph >>= print
```

Note that there's no way to send an EOF in School of Haskell, so the above active code will never terminate.

### Conduit

It would be pretty strange to have a library in conduit-extra that didn't provide some conduit capabilities. You can additionally get a `Sink` to be used to feed data into the process via standard input, and `Source`s for consuming standard output and error.

This next example reads standard input from the console, process standard output with a conduit, and closes standard error.

```haskell
import           Data.Conduit         (($$))
import qualified Data.Conduit.List    as CL
import           Data.Conduit.Process

main :: IO ()
main = do
    putStrLn "Just wrapping cat. Use Ctrl-D to exit."

    (Inherited, src, ClosedStream, cph) <-
        streamingProcess (shell "cat")

    src $$ CL.mapM_ print

    waitForStreamingProcess cph >>= print
```

Note that these `Source`s and `Sink`s will *never* close their `Handle`s. This is done on purpose, to allow them to be used multiple times without accidentally closing their streams. In many cases, you'll need to close the streams manually, which brings us to our next section.

### Conduit + close

Let's say we'd like to close our input stream whenever the user types in "quit". To do that, we need to get an action to close the standard input `Handle`. This is simple: instead of just returning a `Source` or `Sink`, we ask for a tuple of a `Source`/`Sink` together with an `IO ()` action to close the handle.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString      (ByteString)
import           Data.Conduit         (Source, await, yield, ($$), ($=))
import qualified Data.Conduit.Binary  as CB
import           Data.Conduit.Process
import           System.IO            (stdin)

userInput :: Source IO ByteString
userInput =
       CB.sourceHandle stdin
    $= CB.lines
    $= loop
  where
    loop = do
        mbs <- await
        case mbs of
            Nothing -> return ()
            Just "quit" -> return ()
            Just bs -> do
                yield bs
                yield "\n"
                loop

main :: IO ()
main = do
    putStrLn "Just wrapping cat. Type \"quit\" to exit."

    ((sink, close), Inherited, ClosedStream, cph) <-
        streamingProcess (shell "cat")

    userInput $$ sink
    close

    waitForStreamingProcess cph >>= print
```

### UseProvidedHandle

Let's take a quick detour from our running example to talk about the last special type: `UseProvidedHandle`. This says to `streamingProcess`: use the example value of `UseHandle` provided in `std_in`/`std_out`/`std_err`. We can use this to redirect output directly to a file:

```haskell
import Data.Conduit.Process
import System.IO (withFile, IOMode (..))

main :: IO ()
main = do
    let fp = "date.txt"
    withFile fp WriteMode $ \h -> do
        (ClosedStream, UseProvidedHandle, ClosedStream, cph) <-
            streamingProcess (shell "date")
                { std_out = UseHandle h
                }
        waitForStreamingProcess cph >>= print
    readFile fp >>= print
```

Note that this type breaks the type safety a bit, as it requires synchronization between the values provided in the `CreateProcess` value and the return type. This was a conscious trade-off to keep the Data.Conduit.Process API as close as possible to System.Process.

## Use with async

In our examples above, we only ever used a single `Source` or `Sink` at a time. There's a good reason for this: we can easily run into deadlocks if we don't properly handle concurrency. There are multiple ways to do this, but I'm going to strongly recommend using the async package, which handles many corner cases automatically. In particular, the `Concurrently` data type and its `Applicative` instance make it easy and safe to handle multiple streams at once.

Instead of duplicating it here, I'll ask the reader to please refer back to the synopsis example, which ties this all together with two threads for handling streams, and another thread which blocks waiting for the process to exit. That style of concurrency is very idiomatic usage of this library.

## Pipes and signals

When we listed four ways of interacting with a process, that wasn't completely accurate. There are of course many other ways to interact with a process, both directly and indirectly. For example:

* Write to a file that the process will read.
* Open a TCP socket to talk to the process.
* Use up all memory on the system so that the process can't allocate memory.

But there are in particular two ways of interacting with a child process that are very common on POSIX systems: sending signals (such as termination), and connecting over a pipe. The example below demonstrates these two methods, together with the previously mentioned four.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative      ((*>))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (yield, ($$))
import           Data.Conduit.Binary      (sourceHandle)
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process
import           System.Posix.IO          (closeFd, createPipe, fdToHandle)

main :: IO ()
main = do
    (reader, writer) <- createPipe

    writeFile "script.sh" $ unlines
        [ "trap 'echo Got TERM; exit 42' TERM"
        , "echo Sending to standard output"
        , "echo Sending to standard error >&2"
        , "echo Sending to our pipe >&" ++ show writer
        , "cat # Print everything from stdin to stdout"
        , "while true; do sleep 1; done"
        ]

    readerH <- fdToHandle reader

    ((input, close), out, err, cph)
        <- streamingProcess (proc "bash" ["script.sh"])

    closeFd writer

    let go src name =
            Concurrently $ src
            $$ CL.mapM_ (\bs -> putStrLn $ name ++ ": " ++ show bs)
        feed = Concurrently $ do
            threadDelay 1000000
            yield "Feeding standard input, then terminating" $$ input
            terminateProcess $ streamingProcessHandleRaw cph
            close

    runConcurrently
         $ go out "stdout"
        *> go err "stderr"
        *> go (sourceHandle readerH) "pipe"
        *> feed
        *> Concurrently (waitForStreamingProcess cph >>= print)
```

Don't be too worried if some of the details here- especially the shell script- are unfamiliar. This is just meant to demonstrate that Data.Conduit.Process does not get in the way of any standard process interaction.
