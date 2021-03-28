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
[this presentation on conduit](https://www.snoyman.com/reveal/conduit-yesod#/).
Feel free to ignore the `yesod` section.

__NOTE__ As of March 2018, this document has been updated to be
compatible with version 1.3 of conduit. This is available in Long Term
Support (LTS) Haskell version 11 and up. For more information on
changes between versions 1.2 and 1.3,
[see the changelog](https://github.com/snoyberg/conduit/blob/master/conduit/ChangeLog.md#130).

## Table of Contents ##
1. [Synopsis](#synopsis)
2. [Libraries](#libraries)
3. [Conduit as a bad list](#conduit-as-a-bad-list)
4. [Interleaved effects](#interleaved-effects)
5. [Terminology and concepts](#terminology-and-concepts)
6. [Folds](#folds)
7. [Transformations](#transformations)
8. [Monadic composition](#monadic-composition)
9. [Primitives](#primitives)
10. [Evaluation strategy](#evaluation-strategy)
11. [Resource allocation](#resource-allocation)
12. [Chunked data](#chunked-data)
13. [ZipSink](#zipsink)
14. [ZipSource](#zipsource)
15. [ZipConduit](#zipconduit)
16. [Forced consumption](#forced-consumption)
17. [FAQs](#faqs)
18. [More exercises](#more-exercises)
19. [Legacy syntax](#legacy-syntax)
20. [Further reading](#further-reading)

## Synopsis

Basic examples of conduit usage, much more to follow!

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
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
[conduit](https://www.stackage.org/package/conduit) library itself,
which provides a large number of common functions built-in. There is
also the
[conduit-extra](https://www.stackage.org/package/conduit-extra)
library, which adds in some common extra support, like GZIP
(de)compression.

You can run the examples in this tutorial as
[Stack scripts](https://haskell-lang.org/tutorial/stack-script).

## Conduit as a bad list

Let's start off by comparing conduit to normal lists. We'll be able to
compare and contrast with functions you're already used to working
with.

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

take10List :: IO ()
take10List = print
    $ take 10 [1..]

take10Conduit :: IO ()
take10Conduit = print $ runConduitPure
    $ yieldMany [1..] .| takeC 10 .| sinkList

main :: IO ()
main = do
    putStrLn "List version:"
    take10List
    putStrLn ""
    putStrLn "Conduit version:"
    take10Conduit
```

Our list function is pretty straightforward: create an infinite list
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
  disambiguate the names. (If you'd prefer to use a qualified import,
  check out
  [Data.Conduit.Combinators](https://www.stackage.org/haddock/lts/conduit/Data-Conduit-Combinators.html)).
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
-- stack script --resolver lts-12.21
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

complicatedList :: IO ()
complicatedList = print
    $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]

complicatedConduit :: IO ()
complicatedConduit = print $ runConduitPure
     $ yieldMany [1..]
    .| takeC 10
    .| mapC (* 2)
    .| takeWhileC (< 18)
    .| sinkList

main :: IO ()
main = do
    putStrLn "List version:"
    complicatedList
    putStrLn ""
    putStrLn "Conduit version:"
    complicatedConduit
```

Nothing more magical going on, we're just looking at more
functions. For our last bad-list example, let's move over from a pure
pipeline to one which performs some side effects. Instead of
`print`ing the whole result list, let's use `mapM_C` to print each
value individually.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

complicatedList :: IO ()
complicatedList = mapM_ print
    $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]

complicatedConduit :: IO ()
complicatedConduit = runConduit
     $ yieldMany [1..]
    .| takeC 10
    .| mapC (* 2)
    .| takeWhileC (< 18)
    .| mapM_C print

main :: IO ()
main = do
    putStrLn "List version:"
    complicatedList
    putStrLn ""
    putStrLn "Conduit version:"
    complicatedConduit
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
-- stack script --resolver lts-12.21
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

magic :: Int -> IO Int
magic x = do
    putStrLn $ "I'm doing magic with " ++ show x
    return $ x * 2

magicalList :: IO ()
magicalList =
    mapM magic (take 10 [1..]) >>= mapM_ print . takeWhile (< 18)

magicalConduit :: IO ()
magicalConduit = runConduit
     $ yieldMany [1..]
    .| takeC 10
    .| mapMC magic
    .| takeWhileC (< 18)
    .| mapM_C print

main :: IO ()
main = do
    putStrLn "List version:"
    magicalList
    putStrLn ""
    putStrLn "Conduit version:"
    magicalConduit
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
-- stack script --resolver lts-12.21
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
`mapM`, and `mapM_` ourselves, and the solution is less compositional.
Conduit makes it easy to get the right behavior: interleaved
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

In this snippet, `yieldMany [1..10]`, `mapC show`, and `mapM_C print`
are each components. We use the `.|` operator&mdash;a synonym for the
[`fuse` function](https://www.stackage.org/haddock/lts/conduit/Data-Conduit.html#v:fuse)&mdash;to
compose these components into a pipeline. Then we run that pipeline
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
yieldMany [1..10] :: ConduitT ()  Int    IO ()
mapC show         :: ConduitT Int String IO ()
```

There are four type parameters to `ConduitT`:

* The first indicates the upstream value, or input. For `yieldMany`,
  we're using `()`, though really it could be any type since we never
  read anything from upstream. For `mapC`, it's `Int`
* The second indicates the downstream value, or output. For
  `yieldMany`, this is `Int`. Notice how this matches the input of
  `mapC`, which is what lets us combine these two. The output of
  `mapC` is `String`.
* The third indicates the base monad, which tells us what kinds of
  effects we can perform. A `ConduitT` is a monad transformer, so you
  can use `lift` to perform effects. (We'll learn more about conduit's
  monadic nature later.) We're using `IO` in our example.
* The final indicates the result type of the component. This is
  typically only used for the most downstream component in a
  pipeline. We'll get into this when we discuss folds below.

Let's also look at the type of our `.|` operator:

```haskell
(.|) :: Monad m
     => ConduitT a b m ()
     -> ConduitT b c m r
     -> ConduitT a c m r
```

This shows us that:

* The output from the first component must match the input from the
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
runConduit :: Monad m => ConduitT () Void m r -> m r
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
can give the stronger `Void` guarantee in the output case. The long
explanation can be found [here](https://www.fpcomplete.com/blog/2017/07/to-void-or-to-void).

Finally, we talked about pure pipelines before. Those are just
pipelines with `Identity` as the base monad:

```haskell
runConduitPure :: ConduitT () Void Identity r -> r
```

## Folds

A common activity with lists is folding down to a single result. This
concept translates directly into conduit, and works nicely at ensuring
constant memory usage. If you're familiar with folding over lists, the
concepts here should be pretty straightforward, so this will mostly
just be a collection of examples.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..100 :: Int] .| sumC
```

Summing is straightforward, and can be done if desired with the
`foldlC` function:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..100 :: Int] .| foldlC (+) 0
```

You can use `foldMapC` to fold monoids together:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit
import Data.Monoid (Sum (..))

main :: IO ()
main = print $ getSum $ runConduitPure $ yieldMany [1..100 :: Int] .| foldMapC Sum
```

Or you can use `foldC` as a shortened form of `foldMapC id`:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
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
-- stack script --resolver lts-12.21
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
-- stack script --resolver lts-12.21
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
-- stack script --resolver lts-12.21
import Conduit

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

## Transformations

When learning lists, one of the first functions you'll see is `map`,
which transforms each element of the list. We've already seen `mapC`,
above, which does the same thing for conduit. This is just one of many
functions available for performing transformations. Like folds, these
functions are named and behave like their list counterparts in many
examples, so we'll just blast through some examples.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduit $ yieldMany [1..10] .| mapC (* 2) .| mapM_C print
```

We can also filter out values:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduit $ yieldMany [1..10] .| filterC even .| mapM_C print
```

Or if desired we can add some values between each value in the list:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduit $ yieldMany [1..10] .| intersperseC 0 .| mapM_C print
```

It's also possible to "flatten out" a conduit, by converting a stream
of chunks (like a list of vector) of data into the individual values.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduit
     $ yieldMany (map (replicate 5) [1..10])
    .| concatC
    .| mapM_C print
```

__NOTE__ This is our first exposure to "chunked data" in conduit. This
is actually a very important and common use case, especially around
`ByteString`s and `Text`s. We'll cover it in much more detail in its
own section later.

You can also perform monadic actions while transforming. We've seen
`mapMC` being used already, but other such functions exist:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import Conduit

evenM :: Int -> IO Bool
evenM i = do
    let res = even i
    print (i, res)
    return res

main :: IO ()
main = runConduit
     $ yieldMany [1..10]
    .| filterMC evenM
    .| mapM_C print
```

Or you can use the `iterM` function, which performs a monadic action
on the upstream values without modifying them:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Conduit

main :: IO ()
main = do
    res <- runConduit $ yieldMany [1..10] .| iterMC print .| sumC
    print res
```

__EXERCISE__ Implement `iterMC` in terms of `mapMC`.

## Monadic composition

We've so far only really explored half of the power of conduit: being
able to combine multiple components together by connecting the output
of the upstream to the input of the downstream (via the `.|` operator
or the `fuse` function). However, there's another way to combine
simple conduits into more complex ones, using the standard monadic
interface (or `do`-notation). Let's start with some examples,
beginning with a data producer:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Conduit

source :: Monad m => ConduitT i Int m ()
source = do
    yieldMany [1..10]
    yieldMany [11..20]

main :: IO ()
main = runConduit $ source .| mapM_C print
```

We've created a new conduit, `source`, which combines together two
calls to `yieldMany`. Try to guess at intuitively what this will do
before reading the explanation.

As you may have guessed, this program will print the numbers 1
through 20. What we've seen here is that, when you use monadic
composition, the output from the first component is sent downstream,
and then the output from the second component is sent downstream. Now
let's look at the consuming side. Again, try to guess what this
program will do before you read the explanation following it.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Conduit

sink :: Monad m => ConduitT Int o m (String, Int)
sink = do
    x <- takeC 5 .| mapC show .| foldC
    y <- sumC
    return (x, y)

main :: IO ()
main = do
    let res = runConduitPure $ yieldMany [1..10] .| sink
    print res
```

Let's first analyze `takeC 5 .| mapC show .| foldC`. This bit will
take 5 elements from the stream, convert them to `String`s, and then
combine those `String`s into one `String`. So if we actually have 10
elements on the stream, what happens to the other 5? Well, up until
now, the answer would have been "disappears into the aether." However,
we've now introduced monadic composition. In this world, those values
are still sitting on the stream, ready to be consumed by whatever
comes next. In our case, that's `sumC`.

__EXERCISE__ Rewrite `sink` to not use `do`-notation. Hint: it'll be
easier to go `Applicative`.

So we've seen how monadic composition works with both upstream and
downstream, but in isolation. We can just as easily combine these two
concepts together, and create a transformer using monadic composition.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Conduit

trans :: Monad m => ConduitT Int Int m ()
trans = do
    takeC 5 .| mapC (+ 1)
    mapC (* 2)

main :: IO ()
main = runConduit $ yieldMany [1..10] .| trans .| mapM_C print
```

Here, we've set up a conduit that takes the first 5 values it's given,
adds 1 to each, and sends the result downstream. Then, it takes
everything else, multiplies it by 2, and sends it downstream.

__EXERCISE__ Modify `trans` so that it does something different for
the first 3, second 3, and final 3 values from upstream, and drops all
other values.

The only restriction we have in monadic composition is exactly what
you'd expect from the types: the first three type parameters (input,
output, and monad) must be the same for all components.

## Primitives

We've worked with high-level functions in conduit so far. However, at
its core conduit is built on top of a number of simple
primitives. Combined with monadic composition, we can build up all of
the more advanced functions from these primitives. Let's start with
likely the more expected one: `yield`. It's just like the `yieldMany`
function we've been using until now, except it works in a single value
instead of a collection of them.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Conduit

main :: IO ()
main = runConduit $ yield 1 .| mapM_C print
```

Of course, we're not limited to using just a single call to `yield`:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Conduit

main :: IO ()
main = runConduit $ (yield 1 >> yield 2) .| mapM_C print
```

__EXERCISE__ Reimplement `yieldMany` for lists using the `yield`
primitive and monadic composition.

Given that `yield` sends an output value downstream, we also need a
function to get an input value from upstream. For that, we'll use
`await`. Let's start really simple:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = do
    -- prints: Just 1
    print $ runConduitPure $ yield 1 .| await
    -- prints: Nothing
    print $ runConduitPure $ yieldMany [] .| await

    -- Note, that the above is equivalent to the following. Work out
    -- why this works:
    print $ runConduitPure $ return () .| await
    print $ runConduitPure await
```

`await` will ask for a value from upstream, and return a `Just` if
there is a value available. If not, it will return a `Nothing`.

__NOTE__ I was specific in my phrasing of "`await` will ask." This has
to do with the evaluation of a conduit pipeline, and how it is driven
by downstream. We'll cover this in more detail in the next section.

Of course, things get much more interesting when we combine both
`yield` and `await` together. For example, we can implement our own
`mapC` function:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

myMapC :: Monad m => (i -> o) -> ConduitT i o m ()
myMapC f =
    loop
  where
    loop = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just x -> do
                yield (f x)
                loop

main :: IO ()
main = runConduit $ yieldMany [1..10] .| myMapC (+ 1) .| mapM_C print
```

__EXERCISE__ Try implementing `filterC` and `mapMC`. For the latter,
you'll need to use the `lift` function.

The next primitive requires a little motivation. Let's look at a
simple example of using the `takeWhileC` function:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..10] .| do
    x <- takeWhileC (<= 5) .| sinkList
    y <- sinkList
    return (x, y)
```

As you may guess, this will result in the output
`([1,2,3,4,5],[6,7,8,9,10])`. Awesome. Let's go ahead and try to
implement our own `takeWhileC` with just `await` and `yield`.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

myTakeWhileC :: Monad m => (i -> Bool) -> ConduitT i i m ()
myTakeWhileC f =
    loop
  where
    loop = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just x
                | f x -> do
                    yield x
                    loop
                | otherwise -> return ()

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..10] .| do
    x <- myTakeWhileC (<= 5) .| sinkList
    y <- sinkList
    return (x, y)
```

I'd recommend looking over `myTakeWhileC` and making sure you're
comfortable with what it's doing. When you've done that, run the
program and compare the output. To make it easier, I'll put the output
of the original (with the real `takeWhileC`) vs this program:

```
takeWhileC:
([1,2,3,4,5],[6,7,8,9,10])
myTakeWhileC:
([1,2,3,4,5],[7,8,9,10])
```

What happened to `6`? Well, in the `otherwise` branch of the case
statement, we've determined that the value that we received from
upstream does not match our predicate function `f`. So what do we do
with it? Well, we just throw it away! In our program, the first value
to fail the predicate is `6`, so it's discarded, and then our second
`sinkList` usage grabs the _next_ value, which is `7`.

What we need is a primitive that let's us put a value back on the
stream. And we have one that does just that: `leftover`. Let's fix up
our `myTakeWhileC`:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

myGoodTakeWhileC :: Monad m => (i -> Bool) -> ConduitT i i m ()
myGoodTakeWhileC f =
    loop
  where
    loop = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just x
                | f x -> do
                    yield x
                    loop
                | otherwise -> leftover x

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..10] .| do
    x <- myGoodTakeWhileC (<= 5) .| sinkList
    y <- sinkList
    return (x, y)
```

As expected, this has the same output as using the real `takeWhileC`
function.

__EXERCISE__ Implement a `peek` function that gets the next value from
upstream, if available, and then puts it back on the stream.

We can also call `leftover` as many times as we want, and even use
values that didn't come from upstream, though this is a fairly unusual
use case. Just to prove it's possible though:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = print $ runConduitPure $ return () .| do
    mapM_ leftover [1..10]
    sinkList
```

There are two semi-advanced concepts to get across in this example:

1. If you run this, the result is a _descending_ list from 10
   to 1. This is because using `leftover` works in a LIFO (last in
   first out) fashion.
2. If you take off the `return () .|` bit, this example will fail to
   compile. That's because, by using `leftover`, we've stated that our
   conduit actually takes some input from upstream. If you remember,
   when you use `runConduitPure`, the complete pipeline cannot be
   expected any input (it must have an input of type `()`). Adding
   `return () .|` says "we're connecting you to an empty upstream
   component" so satisfy the type system.

## Evaluation strategy

Let's talk about the evaluation strategy of a conduit pipeline. The
most important thing to remember is _everything is driven by
downstream_. To see what I mean, consider this example:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduit $ yieldMany [1..10] .| iterMC print .| return ()
```

This program will generate no output. The reason is that the most
downstream component is `return ()`, which never `await`s any values
from upstream and immediately exits. Once it exits, the entire
pipeline exits. As a result, the two upstream components are never run
at all. If you wanted to instead force all of the values and just
discard them, you could use `sinkNull`:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduit $ yieldMany [1..10] .| iterMC print .| sinkNull
```

Now try and guess what the following program outputs:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduit $ yieldMany [1..10] .| iterMC print .| return () .| sinkNull
```

Answer: nothing! The `sinkNull` will `await` for all values from its
immediate upstream. But its immediate upstream is `return ()`, which
never `yield`s any value, causing the `sinkNull` to exit immediately.

Alright, let's tweak this slightly: what will this one output:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduit
     $ yieldMany [1..10]
    .| iterMC print
    .| liftIO (putStrLn "I was called")
    .| sinkNull
```

In this case, `sinkNull` calls `await`, which forces execution to
defer to the next upstream component (the `liftIO ...` bit). In order
to see if it `yield`s, that component must be evaluated until it
either (1) exits, (2) `yield`s, or (3) `await`s. We see that it exits
after calling `liftIO`, causing the pipeline to terminate, but not
before it prints its "I was called" message.

There's really not too much to understanding conduit evaluation. It
mostly works the way you'd expect, as long as you remember that
_downstream drives_.

## Resource allocation

Let's copy a file with conduit:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit
import qualified System.IO as IO

main :: IO ()
main = IO.withBinaryFile "input.txt" IO.ReadMode $ \inH ->
       IO.withBinaryFile "output.txt" IO.WriteMode $ \outH ->
       runConduit $ sourceHandle inH .| sinkHandle outH
```

This works nicely, and follows the typical bracket pattern we
typically expect in Haskell. However, it's got some downsides:

* You have to allocate all of your resources outside of the conduit
  pipeline. (This is because conduit is coroutine based, and
  coroutines/continuations cannot guarantee a cleanup action is
  called.)
* You will sometimes end up needing to allocate too many resources, or
  holding onto them for too long, if you allocate them in advance
  instead of on demand.
* Some control flows are impossible. For example, if you wanted to
  write a function to traverse a directory tree, you can't open up all
  of the directory handles before you enter your conduit pipeline.

One slight improvement we can make is to switch over to the
`withSourceFile` and `withSinkFile` helper functions, which handle the
calls to `withBinaryFile` for you:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = withSourceFile "input.txt" $ \source ->
       withSinkFile "output.txt" $ \sink ->
       runConduit $ source .| sink
```

However, this only slightly improves ergonomics; the most of the
problems above remain. To solve those (and some others), conduit
provides built in support for a related package
([resourcet](https://www.stackage.org/package/resourcet)), which
allows you to allocate resources and be guaranteed that they will be
cleaned up. The basic idea is that you'll have a block like:

```haskell
runResourceT $ do
    foo
    bar
    baz
```

Any resources that `foo`, `bar`, or `baz` allocate have a cleanup
function registered in a mutable map. When the `runResourceT` call
exits, all of those cleanup functions are called, regardless of
whether the exiting occurred normally or via an exception.

In order to do this in a conduit, we have the built-in function
`bracketP`, which takes an allocation function and a cleanup function,
and provides you a resource. Putting this all together, we can rewrite
our example as:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit
import qualified System.IO as IO
import Data.ByteString (ByteString)

sourceFile' :: MonadResource m => FilePath -> ConduitT i ByteString m ()
sourceFile' fp =
    bracketP (IO.openBinaryFile fp IO.ReadMode) IO.hClose sourceHandle

sinkFile' :: MonadResource m => FilePath -> ConduitT ByteString o m ()
sinkFile' fp =
    bracketP (IO.openBinaryFile fp IO.WriteMode) IO.hClose sinkHandle

main :: IO ()
main = runResourceT
     $ runConduit
     $ sourceFile' "input.txt"
    .| sinkFile' "output.txt"
```

But that's certainly too tedious. Fortunately, conduit provides the
`sourceFile` and `sinkFile` functions built in, and defines a helper
`runConduitRes` which is just `runResourceT . runConduit`. Putting all
of that together, copying a file becomes absolutely trivial:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduitRes $ sourceFile "input.txt" .| sinkFile "output.txt"
```

Let's get a bit more inventive though. Let's traverse an entire
directory tree and write the contents of all files with a `.hs` file
extension into the file "all-haskell-files".

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit
import System.FilePath (takeExtension)

main :: IO ()
main = runConduitRes
     $ sourceDirectoryDeep True "."
    .| filterC (\fp -> takeExtension fp == ".hs")
    .| awaitForever sourceFile
    .| sinkFile "all-haskell-files"
```

What's great about this example is:

* It guarantees that only two file handles are open at a time: the
  `all-haskell-files` destination file and whichever file is being
  read from.
* It will only open as many directory handles as needed to traverse
  the depth of the file structure.
* If any exceptions occur, all resources will be cleaned up.

## Chunked data

I'd like to read a file, convert all of its characters to upper case,
and then write it to standard output. That looks pretty
straightforward:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit
import qualified Data.Text as T
import Data.Char (toUpper)

main :: IO ()
main = runConduitRes
     $ sourceFile "input.txt"
    .| decodeUtf8C
    .| mapC (T.map toUpper)
    .| encodeUtf8C
    .| stdoutC
```

This works just fine, but is inconvenient: isn't that `mapC (T.map
...)` repetition just completely jarring? The issue is that instead of
having a stream of `Char` values, we have a stream of `Text` values,
and our `mapC` function will work on the `Text`s. But our `toUpper`
function works on the `Char`s inside of the `Text`. We want to use
`Text` (or `ByteString`, or sometimes `Vector`) because it's a more
efficient representation of data, but don't want to have to deal with
this overhead.

This is where the chunked functions in conduit come into play. In
addition to functions that work directly on the values in a stream, we
have functions that work on the _elements_ inside those values. These
functions get a `CE` suffix instead of `C`, and are very
straightforward to use. To see it in action:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit
import Data.Char (toUpper)

main :: IO ()
main = runConduitRes
     $ sourceFile "input.txt"
    .| decodeUtf8C
    .| omapCE toUpper
    .| encodeUtf8C
    .| stdoutC
```

__NOTE__ We also had to prepend `o` to get the monomorphic mapping
function, since `Text` is a monomorphic container.

We can use this for other things too. For example, let's get just the
first line of content:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit
import Data.Char (toUpper)

main :: IO ()
main = runConduitRes
     $ sourceFile "input.txt"
    .| decodeUtf8C
    .| takeWhileCE (/= '\n')
    .| encodeUtf8C
    .| stdoutC
```

Or just the first 5 bytes:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduitRes
     $ sourceFile "input.txt"
    .| takeCE 5
    .| stdoutC
```

There are many other functions available for working on chunked
data. In fact, most non-chunked functions have a chunked
equivalent. This means that most of the intuition you've built up for
working with streams of values will automatically translate to dealing
with chunked streams, a big win for binary and textual processing.

__EXERCISE__ Try to implement the `takeCE` function on
`ByteString`s. Hint: you'll need to use `leftover` to make it work
correctly!

## ZipSink

So far we've had very linear pipelines: a component feeds into exactly
one downstream component, and so on. However, sometimes we may wish to
allow for multiple consumers of a single stream. As a motivating
example, let's consider taking the average of a stream of
`Double`s. In the list world, this may look like:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
doubles :: [Double]
doubles = [1, 2, 3, 4, 5, 6]

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

main :: IO ()
main = print $ average doubles
```

However, performance aficionados will quickly point out that this has
a space leak: the list will be traversed once for the `sum`, kept in
memory, and then traversed a second time for the `length`. We could
work around that by using lower-level functions, but we lose
composability. (Though see the
[foldl package](https://www.stackage.org/package/foldl) for composable
folding.)

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

doubles :: [Double]
doubles = [1, 2, 3, 4, 5, 6]

average :: Monad m => ConduitT Double Void m Double
average =
    getZipSink (go <$> ZipSink sumC <*> ZipSink lengthC)
  where
    go total len = total / fromIntegral len

main :: IO ()
main = print $ runConduitPure $ yieldMany doubles .| average
```

`ZipSink` is a newtype wrapper which provides an different
`Applicative` instance than the standard one for `ConduitT`. Instead
of sequencing the consumption of a stream, it allows two components to
consume _in parallel_. Now, our `sumC` and `lengthC` are getting
values at the same time, and then those values can be immediately
thrown away. This leads to easy composition and constant memory usage.

__NOTE__ Both the list and conduit versions of this are subject to a
divide-by-zero error. You'd probably in practice want to make
`average` return a `Maybe Double`.

Another real world example of `ZipSink` is when you want to both
consume a file and calculate its cryptographic hash. Working with the
`cryponite` and `cryptonite-conduit` libraries:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit
import Crypto.Hash.Conduit (sinkHash)
import Crypto.Hash (Digest, SHA256)

main :: IO ()
main = do
    digest <- runConduitRes
            $ sourceFile "input.txt"
           .| getZipSink (ZipSink (sinkFile "output.txt") *> ZipSink sinkHash)
    print (digest :: Digest SHA256)
```

Or we can get slightly more inventive, and read from an HTTP connection instead of a local file:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import Conduit
import Crypto.Hash.Conduit (sinkHash)
import Crypto.Hash (Digest, SHA256)
import Network.HTTP.Simple (httpSink)

main :: IO ()
main = do
    digest <- runResourceT $ httpSink "http://httpbin.org"
              (\_res -> getZipSink (ZipSink (sinkFile "output.txt") *> ZipSink sinkHash))
    print (digest :: Digest SHA256)
```

This provides a convenient and efficient method to consume data over a
network connection.

## ZipSource

Let's keep a good thing going. In addition to consuming in parallel,
we may wish to produce in parallel. For this, we'll use the
`ZipSource` newtype wrapper, which is very similar in concept to the
`ZipList` wrapper for those familiar. As a simple example, let's
create a stream of the Fibonacci numbers, together with each one's
index in the sequence:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

indexedFibs :: ConduitT () (Int, Int) IO ()
indexedFibs = getZipSource
    $ (,)
  <$> ZipSource (yieldMany [1..])
  <*> ZipSource (yieldMany fibs)

main :: IO ()
main = runConduit $ indexedFibs .| takeC 10 .| mapM_C print
```

## ZipConduit

To round out the collection of newtype wrappers, we've got
`ZipConduit`, which is certainly the most complicated of the bunch. It
allows you to combine a bunch of transformers in such a way that:

* Drain all of the `ZipConduit`s of all `yield`ed values, until they
  are all `await`ing
* Grab the next value from upstream, and feed it to all of the
  `ZipConduit`s
* Repeat

Here's a silly example of using it, which demonstrates its most common
use case: focusing in on a subset of a stream. We split a stream of
numbers into evens (`Left`) and odds (`Right`). Then we have two
transformers that each look at only half the stream, and combine those
two transformers together into a single transformer that looks at the
whole stream:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

tagger :: Monad m => ConduitT Int (Either Int Int) m ()
tagger = mapC $ \i -> if even i then Left i else Right i

evens, odds :: Monad m => ConduitT Int String m ()
evens  = mapC $ \i -> "Even number: " ++ show i
odds   = mapC $ \i -> "Odd  number: " ++ show i

left :: Either l r -> Maybe l
left = either Just (const Nothing)

right :: Either l r -> Maybe r
right = either (const Nothing) Just

inside :: Monad m => ConduitT (Either Int Int) String m ()
inside = getZipConduit
    $ ZipConduit (concatMapC left  .| evens)
   *> ZipConduit (concatMapC right .| odds)

main :: IO ()
main = runConduit $ enumFromToC 1 10 .| tagger .| inside .| mapM_C putStrLn
```

In my experience, the most useful of the three newtype wrappers is
`ZipSink`, but your mileage may vary.

## Forced consumption

Remember that, in our evaluation method for conduit, we stop
processing as soon as downstream stops. There are some cases where
this is problematic, specifically when we want to ensure a specific
amount of data is consumed. Consider:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

withFiveSum :: Monad m
            => ConduitT Int o m r
            -> ConduitT Int o m (r, Int)
withFiveSum inner = do
    r <- takeC 5 .| inner
    s <- sumC
    return (r, s)

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..10] .| withFiveSum sinkList
```

Our `withFiveSum` function will let the provided `inner` conduit work
on the first five values in the stream, then take the sum of the
rest. All seems well, but now consider if we replace `sinkList` with
`return ()`. Our `takeC 5 .| return ()` will no longer consume any of
the first five values, and `sumC` will end up consuming
them. Depending on your use case, this could be problematic, and very
surprising.

We can work around this by forcing all other values to be dropped,
e.g.:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

withFiveSum :: Monad m
            => ConduitT Int o m r
            -> ConduitT Int o m (r, Int)
withFiveSum inner = do
    r <- takeC 5 .| do
        r <- inner
        sinkNull
        return r
    s <- sumC
    return (r, s)

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..10] .| withFiveSum (return ())
```

However, there's also a convenience function which captures this
pattern: `takeExactlyC`:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

withFiveSum :: Monad m
            => ConduitT Int o m r
            -> ConduitT Int o m (r, Int)
withFiveSum inner = do
    r <- takeExactlyC 5 inner
    s <- sumC
    return (r, s)

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..10] .| withFiveSum (return ())
```

Notice that there's no `.|` operator between `takeExactlyC 5` and
`inner`. That's not a typo! `takeExactlyC` isn't actually a conduit,
it's a _combinator_ which, when given a conduit, will generate a
conduit.

__EXERCISE__ Try to write `takeExactlyC` as a conduit itself, and/or
convince yourself why that's impossible.

This same kind of pattern is used to deal with the stream-of-streams
problem. As a motivating example, consider processing a file, and
wanting to work on it one line at a time. One possibility is to simply
break the stream into one `Text` per line, but this can be dangerous
if your input is untrusted and may contain an unbounded line
length. Instead, we can just do:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduitRes $ sourceFile "input.txt" .| decodeUtf8C .| do
    len <- lineC lengthCE
    liftIO $ print len
```

This program will print out the length of the first line of the input
file. However, by combining with the `peekForeverE` combinator - which
will continuously run a conduit as long as there is some input
available in a chunked stream - we can print out the length of each
line:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit

main :: IO ()
main = runConduitRes $ sourceFile "input.txt" .| decodeUtf8C .| peekForeverE (do
    len <- lineC lengthCE
    liftIO $ print len)
```

## FAQs

* _How do you deal with an upstream conduit that has a return value?_
  The special fusion functions for it, see
  [the haddocks](https://www.stackage.org/haddock/lts/conduit/Data-Conduit.html#g:6).
* _How do you capture unconsumed leftover values?_ Again, the special
  fusion functions for it, see
  [the haddocks](https://www.stackage.org/haddock/lts/conduit/Data-Conduit.html#g:14).
* _How do I run a source, take some of its output, and then run the
  rest of it later?_
  [Connect and resume](https://www.stackage.org/haddock/lts/conduit/Data-Conduit.html#g:12)

## More exercises

Write a conduit that consumes a stream of `Int`s. It takes the first
`Int` from the stream, and then multiplies all subsequent `Int`s by
that number and sends them back downstream. You should use the `mapC`
function for this.

Take a file and, for each line, print out the number of bytes in the
line (try using bytestring directly and then conduit).

_Further exercises wanted, please feel free to send PRs!_

## Legacy syntax

As of version 1.2.8 of conduit, released September 2016, the above used
operators and function names are recommended. However, prior to that, an
alternate set of functions and operators was used instead. You may still find
code and documentation out there which follows the legacy syntax, so it's worth
being aware of it. Basically:

* Instead of `.|`, we had three operators: `$=`, `=$`, and `=$=`. These were
  all synonyms, and existed for historical reasons.
* The `$$` operator is a combination of `runConduit` and `.|`.

To put it simply in code:

```haskell
x $=  y = x .| y
x =$  y = x .| y
x =$= y = x .| y
x $$  y = runConduit (x .| y)
```

If the old operators seem needlessly confusing/redundant... well, that's why we
have new operators :).

Prior to the 1.3.0 release in February 2018, there were different data
types and type synonyms available. In particular, instead of
`ConduitT`, we had `ConduitM`, and we also had the following synonyms:

```haskell
type Source     m o   =           ConduitM () o    m ()
type Sink     i m   r =           ConduitM i  Void m r
type Conduit  i m o   =           ConduitM i  o    m ()
type Producer   m o   = forall i. ConduitM i  o    m ()
type Consumer i m   r = forall o. ConduitM i  o    m r
```

These older names are all still available, but they've been deprecated
to simplify the package.

## Further reading

Some blogs posts making heavy usage of conduit:

* [network-conduit, async, and conduit-combinators](http://www.yesodweb.com/blog/2014/03/network-conduit-async)
* Practical Haskell: Simple File Mirror
  [part 1](https://www.fpcomplete.com/blog/2016/09/practical-haskell-simple-file-mirror-1)
  and
  [part 2](https://www.fpcomplete.com/blog/2016/09/practical-haskell-simple-file-mirror-2)

_If you have other articles to include, please send a PR!_
