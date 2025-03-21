# ChangeLog for conduit

## 1.3.6.1

* Forward compatibility with `-Wnoncanonical-monad-instances` becoming an error

## 1.3.6

* Avoid dropping upstream items in `mergeSource` [#513](https://github.com/snoyberg/conduit/pull/513)

## 1.3.5

* Add `groupOn`

## 1.3.4.3

* Fix space leak in `*>` [#496](https://github.com/snoyberg/conduit/issues/496) [#497](https://github.com/snoyberg/conduit/pull/497)

## 1.3.4.2

* Fix GHC 9.2 build [#473](https://github.com/snoyberg/conduit/pull/473)

## 1.3.4.1

* Library and tests compile and run with GHC 9.0.1 [#455](https://github.com/snoyberg/conduit/pull/455)

## 1.3.4

* Add `foldWhile` [#453](https://github.com/snoyberg/conduit/issues/453) [#456](https://github.com/snoyberg/conduit/pull/456).

## 1.3.3

* Add `uncons`, `unconsM`, `unconsEither`, `unconsEitherM`.

## 1.3.2.1

* Fix isChunksForExactlyE [#445](https://github.com/snoyberg/conduit/issues/445) [#446](https://github.com/snoyberg/conduit/pull/446)

## 1.3.2

* Add `mapInputM` [#435](https://github.com/snoyberg/conduit/pull/435)

## 1.3.1.2

* More eagerly emit groups in `chunksOf` [#427](https://github.com/snoyberg/conduit/pull/427)

## 1.3.1.1

* Use lower-case imports (better for cross-compilation) [#408](https://github.com/snoyberg/conduit/pull/408)

## 1.3.1

* Add `MonadFail` instance for `ConduitT`.

## 1.3.0.3

* Improve fusion framework rewrite rules

## 1.3.0.2

* Replace `ReadMode` with `WriteMode` in `withSinkFile`

## 1.3.0.1

* Test suite compatibility with GHC 8.4.1 [#358](https://github.com/snoyberg/conduit/issues/358)

## 1.3.0

* Drop monad-control and exceptions in favor of unliftio
* Drop mmorph dependency
* Deprecate old type synonyms and operators
* Drop finalizers from the library entirely
    * Much simpler
    * Less guarantees about prompt finalization
    * No more `yieldOr`, `addCleanup`
    * Replace the `Resumable` types with `SealedConduitT`
* Add the `Conduit` and `Data.Conduit.Combinators` modules, stolen from
  `conduit-combinators`

## 1.2.13

* Add `Semigroup` instances [#345](https://github.com/snoyberg/conduit/pull/345)

## 1.2.12.1

* Fix `pass` in `ConduitM` `MonadWriter` instance

## 1.2.12

* Add `exceptC`, `runExceptC` and `catchExceptC` to `Data.Conduit.Lift`

## 1.2.11

* Add `unfoldEither` and `unfoldEitherM` to `Data.Conduit.List`

## 1.2.10

* Add `PrimMonad` instances for `ConduitM` and `Pipe`
  [#306](https://github.com/snoyberg/conduit/pull/306)

## 1.2.9.1

* Ensure downstream and inner sink receive same inputs in
  `passthroughSink`
  [#304](https://github.com/snoyberg/conduit/issues/304)

## 1.2.9

* `chunksOf` [#296](https://github.com/snoyberg/conduit/pull/296)

## 1.2.8

* Implement
  [the reskinning idea](http://www.snoyman.com/blog/2016/09/proposed-conduit-reskin):
    * `.|`
    * `runConduitPure`
    * `runConduitRes`

## 1.2.7

* Expose yieldM for ConduitM [#270](https://github.com/snoyberg/conduit/pull/270)

## 1.2.6.6

* Fix test suite compilation on older GHCs

## 1.2.6.5

* In zipConduitApp, left bias not respected mixing monadic and non-monadic conduits [#263](https://github.com/snoyberg/conduit/pull/263)

## 1.2.6.4

* Fix benchmark by adding a type signature

## 1.2.6.3

* Doc updates

## 1.2.6.2

* resourcet cannot be built with GHC 8 [#242](https://github.com/snoyberg/conduit/issues/242)
* Remove upper bound on transformers [#253](https://github.com/snoyberg/conduit/issues/253)

## 1.2.6

* `sourceToList`
* Canonicalise Monad instances [#237](https://github.com/snoyberg/conduit/pull/237)

## 1.2.5

* mapAccum and mapAccumM should be strict in their state [#218](https://github.com/snoyberg/conduit/issues/218)

## 1.2.4.1

* Some documentation improvements

## 1.2.4

* [fuseBothMaybe](https://github.com/snoyberg/conduit/issues/199)

__1.2.3__ Expose `connect` and `fuse` as synonyms for `$$` and `=$=`, respectively.

__1.2.2__ Lots more stream fusion.

__1.2__ Two performance optimizations added. (1) A stream fusion framework. This is a non-breaking change. (2) Codensity transform applied to the `ConduitM` datatype. This only affects users importing the `.Internal` module. Both changes are thoroughly described in the following to blog posts: [Speeding up conduit](https://www.fpcomplete.com/blog/2014/08/iap-speeding-up-conduit), and [conduit stream fusion](https://www.fpcomplete.com/blog/2014/08/conduit-stream-fusion).

__1.1__ Refactoring into conduit and conduit-extra packages. Core functionality is now in conduit, whereas most common helper modules (including Text, Binary, Zlib, etc) are in conduit-extra. To upgrade to this version, there should only be import list and conduit file changes necessary.

__1.0__ Simplified the user-facing interface back to the Source, Sink, and Conduit types, with Producer and Consumer for generic code. Error messages have been simplified, and optional leftovers and upstream terminators have been removed from the external API. Some long-deprecated functions were finally removed.

__0.5__ The internals of the package are now separated to the .Internal module, leaving only the higher-level interface in the advertised API. Internally, switched to a `Leftover` constructor and slightly tweaked the finalization semantics.

__0.4__ Inspired by the design of the pipes package: we now have a single unified type underlying `Source`, `Sink`, and `Conduit`. This type is named `Pipe`. There are type synonyms provided for the other three types. Additionally, `BufferedSource` is no longer provided. Instead, the connect-and-resume operator, `$$+`, can be used for the same purpose.

__0.3__ ResourceT has been greatly simplified, specialized for IO, and moved into a separate package. Instead of hard-coding ResourceT into the conduit datatypes, they can now live around any monad. The Conduit datatype has been enhanced to better allow generation of streaming output. The SourceResult, SinkResult, and ConduitResult datatypes have been removed entirely.

__0.2__ Instead of storing state in mutable variables, we now use CPS. A `Source` returns the next `Source`, and likewise for `Sink`s and `Conduit`s. Not only does this take better advantage of GHC\'s optimizations (about a 20% speedup), but it allows some operations to have a reduction in algorithmic complexity from exponential to linear. This also allowed us to remove the `Prepared` set of types. Also, the `State` functions (e.g., `sinkState`) use better constructors for return types, avoiding the need for a dummy state on completion.

__0.1__ `BufferedSource` is now an abstract type, and has a much more efficient internal representation. The result was a 41% speedup on microbenchmarks (note: do not expect speedups anywhere near that in real usage). In general, we are moving towards `BufferedSource` being a specific tool used internally as needed, but using `Source` for all external APIs.

__0.0__ Initial release.
