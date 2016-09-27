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
