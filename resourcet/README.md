## resourcet

Proper exception handling, especially in the presence of asynchronous
exceptions, is a non-trivial task. But such proper handling is absolutely vital
to any large scale application. Leaked file descriptors or database connections
will simply not be an option when writing a popular web application, or a high
concurrency data processing tool. So the question is, how do you deal with it?

The standard approach is the bracket pattern, which appears throughout much of
the standard libraries. `withFile` uses the bracket pattern to safely wrap up
`openFile` and `closeFile`, guaranteeing that the file handle will be closed no
matter what. This approach works well, and I highly recommend using it.

However, there's another approach available: the [resourcet
package](https://www.stackage.org/package/resourcet).  If the bracket pattern
is so good, why do we need another one? The goal of this post is to answer that
question.

## What is ResourceT

ResourceT is a monad transformer which creates a region of code where you can safely allocate resources. Let's write a simple example program: we'll ask the user for some input and pretend like it's a scarce resource that must be released. We'll then do something dangerous (potentially introducing a divide-by-zero error). We then want to immediately release our scarce resource and perform some long-running computation.

```haskell
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

main = runResourceT $ do
    (releaseKey, resource) <- allocate
        (do
            putStrLn "Enter some number"
            readLn)
        (\i -> putStrLn $ "Freeing scarce resource: " ++ show i)
    doSomethingDangerous resource
    liftIO $ putStrLn $ "Going to release resource immediately: " ++ show resource
    release releaseKey
    somethingElse
    
doSomethingDangerous i =
    liftIO $ putStrLn $ "5 divided by " ++ show i ++ " is " ++ show (5 `div` i)
    
somethingElse = liftIO $ putStrLn
    "This could take a long time, don't delay releasing the resource!"
```

Try entering a valid value, such as 3, and then enter 0. Notice that in both cases the "Freeing scarce resource" message it printed. And by using `release` before `somethingElse`, we guarantee that the resource is freed *before* running the potentially long process.

In this specific case, we could easily represent our code in terms of bracket with a little refactoring.

```haskell
import Control.Exception (bracket)

main = do
    bracket
        (do
            putStrLn "Enter some number"
            readLn)
        (\i -> putStrLn $ "Freeing scarce resource: " ++ show i)
        doSomethingDangerous
    somethingElse
doSomethingDangerous i =
    putStrLn $ "5 divided by " ++ show i ++ " is " ++ show (5 `div` i)
    
somethingElse = putStrLn
    "This could take a long time, don't delay releasing the resource!"
```

In fact, the `bracket` version is cleaner than the resourcet version. If so, why bother with resourcet at all? Let's build up to the more complicated cases.

## bracket in terms of ResourceT

The first thing to demonstrate is that `ResourceT` is strictly more powerful than `bracket`, in the sense that:

1. `bracket` can be implemented in terms of `ResourceT`.
2. `ResourceT` cannot be implemented in terms of `bracket`.

The first one is pretty easy to demonstrate:

```haskell
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class

bracket alloc free inside = runResourceT $ do
    (_releaseKey, resource) <- allocate alloc free
    lift $ inside resource
    
main = bracket
    (putStrLn "Allocating" >> return 5)
    (\i -> putStrLn $ "Freeing: " ++ show i)
    (\i -> putStrLn $ "Using: " ++ show i)
```

Now let's analyze why the second statement is true.

## What ResourceT adds

The `bracket` pattern is designed with nested resource allocations. For example, consider the following program which copies data from one file to another. We'll open up the source file using `withFile`, and then nest within it another `withFile` to open the destination file, and finally do the copying with both file handles.

```haskell
{-# START_FILE main.hs #-}
import System.IO
import qualified Data.ByteString as S

main = do
    withFile "input.txt" ReadMode $ \input ->
      withFile "output.txt" WriteMode $ \output -> do
        bs <- S.hGetContents input
        S.hPutStr output bs
    S.readFile "output.txt" >>= S.putStr
{-# START_FILE input.txt #-}
This is the input file.
```

But now, let's tweak this a bit. Instead of reading from a single file, we want to read from two files and concatenate them. We could just have three nested `withFile` calls, but that would be inefficient: we'd have two `Handle`s open for reading at once, even though we'll only ever need one. We could restructure our program a bit instead: put the `withFile` for the output file on the outside, and then have two calls to `withFile` for the input files on the inside.

But consider a more complicated example. Instead of just a single destination file, let's say we want to break up our input stream into chunks of, say, 50 bytes each, and write each chunk to successive output files. We now need to __interleave__ allocations and freeings of both the source and destination files, and we cannot statically know exactly how the interleaving will look, since we don't know the size of the files at compile time.

This is the kind of situation that `resourcet` solves well (we'll demonstrate in the next section). As an extension of this, we can write library functions which allow user code to request arbitrary resource allocations, and we can guarantee that they will be cleaned up. A prime example of this is in WAI (Web Application Interface). The user application may wish to allocate some scarce resources (such as database statements) and use them in the generation of the response body. Using `ResourceT`, the web server can guarantee that these resources will be cleaned up.


## Interleaving with conduit

Let's demonstrate the interleaving example described above. To simplify the code, we'll use the conduit package for the actual chunking implementation. Notice when you run the program that there are never more than two file handles open at the same time.

```haskell
import           Control.Monad.IO.Class (liftIO)
import           Data.Conduit           (addCleanup, runResourceT, ($$), (=$))
import           Data.Conduit.Binary    (isolate, sinkFile, sourceFile)
import           Data.Conduit.List      (peek)
import           Data.Conduit.Zlib      (gzip)
import           System.Directory       (createDirectoryIfMissing)

-- show
-- All of the files we'll read from
infiles = map (\i -> "input/" ++ show i ++ ".bin") [1..10]

-- Generate a filename to write to
outfile i = "output/" ++ show i ++ ".gz"

-- Monad instance of Source allows us to simply mapM_ to create a single Source
-- for reading all of the files sequentially.
source = mapM_ sourceFileTrace infiles

-- The Sink is a bit more complicated: we keep reading 30kb chunks of data into
-- new files. We then use peek to check if there is any data left in the
-- stream. If there is, we continue the process.
sink =
    loop 1
  where
    loop i = do
        isolate (30 * 1024) =$ sinkFileTrace (outfile i)
        mx <- peek
        case mx of
            Nothing -> return ()
            Just _ -> loop (i + 1)

-- Putting it all together is trivial. ResourceT guarantees we have exception
-- safety.
transform = runResourceT $ source $$ gzip =$ sink
-- /show

-- Just some setup for running our test.
main = do
    createDirectoryIfMissing True "input"
    createDirectoryIfMissing True "output"
    mapM_ fillRandom infiles
    transform

fillRandom fp = runResourceT
              $ sourceFile "/dev/urandom"
             $$ isolate (50 * 1024)
             =$ sinkFile fp


-- Modified sourceFile and sinkFile that print when they are opening and
-- closing file handles, to demonstrate interleaved allocation.
sourceFileTrace fp = do
    liftIO $ putStrLn $ "Opening: " ++ fp
    addCleanup (const $ liftIO $ putStrLn $ "Closing: " ++ fp) (sourceFile fp)

sinkFileTrace fp = do
    liftIO $ putStrLn $ "Opening: " ++ fp
    addCleanup (const $ liftIO $ putStrLn $ "Closing: " ++ fp) (sinkFile fp)
```

## resourcet is not conduit

resourcet was originally created in the process of writing the conduit package.
As a result, many people have the impression that these two concepts are
intrinsically linked. In fact, this is not true: each can be used separately
from the other. The canonical demonstration of resourcet combined with conduit
is the file copy function:

```haskell
import Data.Conduit
import Data.Conduit.Binary

fileCopy src dst = runResourceT $ sourceFile src $$ sinkFile dst

main = do
    writeFile "input.txt" "Hello"
    fileCopy "input.txt" "output.txt"
    readFile "output.txt" >>= putStrLn
```

However, since this function does not actually use any of ResourceT's added functionality, it can easily be implemented with the bracket pattern instead:

```haskell
import Data.Conduit
import Data.Conduit.Binary
import System.IO

fileCopy src dst = withFile src ReadMode $ \srcH ->
                   withFile dst WriteMode $ \dstH ->
                   sourceHandle srcH $$ sinkHandle dstH

main = do
    writeFile "input.txt" "Hello"
    fileCopy "input.txt" "output.txt"
    readFile "output.txt" >>= putStrLn
```

Likewise, resourcet can be freely used for more flexible resource management without touching conduit. In other words, these two libraries are completely orthogonal and, while they complement each other nicely, can certainly be used separately.

## Conclusion

ResourceT provides you with a flexible means of allocating resources in an exception safe manner. Its main advantage over the simpler bracket pattern is that it allows interleaving of allocations, allowing for more complicated programs to be created efficiently. If your needs are simple, stick with bracket. If you have need of something more complex, resourcet may be your answer.
