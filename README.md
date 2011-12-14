Conduits are an approach to the streaming data problem. It is meant as an
alternative to enumerators/iterators, hoping to address the same issues with
different trade-offs based on real-world experience with enumerators.

General Goal
===========================

Let's start by defining the goal of enumerators, iterators, and conduits. We
want a standard interface to represent streaming data from one point to
another, possibly modifying the data along the way.

This goal is also achieved by lazy I/O; the problem with lazy I/O, however, is
that of deterministic resource cleanup. That is to say, with lazy I/O, you
cannot be guaranteed that your file handles will be closed as soon as you have
finished reading data from them.

We want to keep the same properties of constant memory usage from lazy I/O, yet
have guarantees that scarce resources will be freed as early as possible.

Enumerator
===========================

__Note__: This is biased towards John Millikin's enumerator package, as that is
the package with which I have the most familiarity.

The concept of an enumerator is fairly simple. We have an `Iteratee` which
"consumes" data. It keeps its state while being fed data by an `Enumerator`.
The `Enumerator` will feed data a few chunks at a time to an `Iteratee`,
transforming the `Iteratee`'s state at each call. Additionally, there is an
`Enumeratee` that acts as both an `Enumerator` and `Iteratee`.

As a result, there are a few changes to code structure that need to take place
in order to fully leverage enumerators:

*    The `Enumerator`s control code flow. This is an Inversion of Control (IoC)
     technique.
     
     __Practical ramification__: `Iteratee` code can be more difficult to
     structure.

*    `Iteratee`s are not able to allocate scarce resources. Since they do not
     have any control of the flow of the program, they cannot guarantee that
     the resources will be released, especially in the presence of exceptions.

     __Practical ramification__: There is no way to create an `iterFile`, which
     will stream data into a file. Instead, you must allocate a file handle
     before entering the `Iteratee` and pass that in. In some cases, such an
     approach would mean file handles are kept open too long.

*    None of this plays nicely with monad transformers, though this does not
     seem to be an inherent problem with enumerators, instead with the current
     library.

     __Practical ramification__: You cannot enumerate a file when running in a
     `ReaderT IO`.

*    Instead of passing around a `Handle` to pull data from, your code should
     live inside an `Iteratee`. This makes it difficult and/or impossible to
     interleave two different sources.

     __Practical ramification__: Even with libraries designed to interoperate
     (like http-enumerator and warp), it is not possible to create a proper
     streaming HTTP proxy.

*    While the concepts are simple, actually writing low-level Iteratee code is
     very complex. This in turn intimidates users from adopting the approach.

Conduits
===========================

Conduits attempt to provide a similar high-level API to enumerators, while
providing a drastically different low-level implementation. The first question
to visit is: why does the enumerator need to control flow of the program? The
main purpose is to ensure that resources are released properly. But this in
fact solved only *half* the problem; iteratees still cannot release resources.

ResourceT
---------------------------

So our first issue to address is to create a new way to deal with resource
allocation. We represent this as a monad transformer, `ResourceT`. It works as
follows:

* You can register a cleanup action, which will return a `ReleaseKey`.

* If you pass your `ReleaseKey` to the `release` function, your action will be
  called automatically, and your action will be unregistered.

* When the monad is exited (via `runRelease`), all remaining registered actions
  will be called.

* All of this is provided in an exception-safe manner.

For example, you would be able to open a file handle, and then register an
action to close the file handle. In your code, you would call `release` on your
`ReleaseKey` as soon as you reach the end of the contents you are streaming. If
that code is never reached, the file handle will be released when the monad
terminates.

Source
---------------------------

Now that we have a way to deal with resources, we can take a radically
different approach to production of data streams. Instead of a push system,
where the enumerators sends data down the pipeline, we have a pull system,
where data is requested from the source. Additionally, a source allows
buffering of input data, so data can be "pushed back" onto the source to be
available for a later call.

Sink
---------------------------

A `Sink` is the corrollary to an `Iteratee`. It takes a stream of data, and can
return a result, consisting of leftover input and an output. Like an
`Iteratee`, a `Sink` provides a `Monad` instance, which allows easy chaining
together of `Sink`s.

However, a big difference is that your code needn't live in the `Sink` monad.
You can easily pass around your sources and connect them to different `Sink`s.
As a practical example, when the Web Application Interface (WAI) is translated
to conduits, the application lives in the `ResourceT IO` monad, and the
`Request` value contains a `requestBody` record, which is a `Source IO
ByteString`.

Conduit
---------------------------

Conduits are simply functions that that a stream of input data and return
leftover input as well as a stream of output data. Conduits are far simpler to
implement than their corrollary, `Enumeratee`s.

Connecting
---------------------------

While you can directly pull data from a `Source`, or directly push to a `Sink`, the easiest approach is to use the built-in connect operators. These follow the naming convention from the enumerator package, e.g.:

    sourceFile "myfile.txt" $$ sinkFile "mycopy.txt"
    sourceFile "myfile.txt" $= uppercase {- a conduit -} $$ sinkFile "mycopy.txt"
    fromList [1..10] $$ Data.Conduit.List.map (+ 1) =$ fold (+) 0

Trade-offs
===========================

Overall, the approach achieves the goals I had hoped for. The main downside in
its current form is its reliance on mutable data. Instead of having an
`Iteratee` return a new `Iteratee`, thereby provide an illusion of mutability,
in conduit the sources and sinks must maintain their state internally. As a
result, code must live in IO and usually use something like an IORef to keep
track of the current state.

I believe this to be an acceptable trade-off, since:

1. Virtually all conduit code will be performing I/O, so staying in the `IO`
   monad is reasonable.
2. By using `monad-control`, conduit can work with any monad *based* on `IO`,
   meaning all standard transformers (except `ContT`) can be used.
3. Enumerator experience has shown that the majority of the time, you construct
   `Iteratee`s by using built-in functions, such as fold and map. Therefore,
   the complication of tracking mutable state will usually be abstracted from
   users.

Another minor point is that, in order to provide an efficient `Monad` instance,
the `Sink` type is complicated with tracking two cases: a `Sink` which expects
data and one which does not. As expressed in point (3) above, this should not
have a major impact for users.

Finally, since most `Source`s and `Sink`s begin their life by allocating some
mutable variable, both types allow some arbitrary monadic action to be run
before actual processing begins. The monad (et al) instances and connect
functions are all built to run this action once and then continue operation.

Status
===========================

This is currently no more than a proof-of-concept, to see the differences
between enumerators and conduits for practical problems. This may serve as a
basis for WAI and Yesod in the future, but that will only be after careful
vetting of the idea. Your input is greatly appreciated!

Notes
===========================

This is just a collection of my personal notes, completely unorganized.

*   In enumerator, it's relatively easy to combined multiple `Iteratee`s into
    an `Enumeratee`. The equivalent (turning `Sink`s into a `Conduit`) is
    harder.  See, for example, chunking in http-conduit. Perhaps this can be
    improved with a better `sequence`.

*   Names and operators are very long right now. Is that a feature or a bug?

*   Should we use Vector in place of lists?

*   It might be worth transitioning to RegionT. Will the extra type parameter
    scare people away?

*   Perhaps the whole BSource/BConduit concept doesn't need to be exposed to
    the user. Advantage of exposing: it makes it obvious at the type level that
    a source/conduit can be reused, and possibly more efficient implementations
    (no double buffering). Disadvantage: more functions to implement/user to
    keep track of, so harder to use.

*   I dislike the travesty which is `type FilePath = [Char]`, so I'm using the
    system-filepath package. I've used it for a lot of internal code at work,
    and it performs wonderfully. If anyone is concerned about this approach,
    let me know.
