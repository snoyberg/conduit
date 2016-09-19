## 1.1.13.3

* `withCheckedProcessCleanup` properly closes opened `Handle`s
  [#280](https://github.com/snoyberg/conduit/issues/280)

## 1.1.13.2

* Fix alignment issues on non-X86 archs

## 1.1.13.1

* Fix an incorrect comment

## 1.1.13

* Add `sinkStorable` and `sinkStorableEx`

## 1.1.12.1

* Fix build for GHC `<= 7.8` [#260](https://github.com/snoyberg/conduit/issues/260)
* Fix accidentally breaking change in `sourceProcessWithConsumer` type signature

## 1.1.12

* Add sourceProcessWithStreams [#258](https://github.com/snoyberg/conduit/pull/258)

## 1.1.11

* `withCheckedProcessCleanup`

## 1.1.10.1

* Fix a leftovers bug in helperDecompress #254

## 1.1.10

* `multiple` combinator for `Data.Conduit.Zlib` [#254](https://github.com/snoyberg/conduit/issues/254)

## 1.1.9.3

* Some typo fixes in docs

## 1.1.9

* detectUtf [#217](https://github.com/snoyberg/conduit/pull/217)

## 1.1.8

*  Adding buffer size to sourceHandleRange [#213](https://github.com/snoyberg/conduit/pull/213)

## 1.1.7.3

* Make Binary.lines O(n) instead of O(n^2) [#209](https://github.com/snoyberg/conduit/pull/209)

## 1.1.7.2

* Fix for: Decompressing a specific amount of zlib data "eats" following data [#20](https://github.com/fpco/streaming-commons/issues/20)

## 1.1.7

Add `Data.Conduit.ByteString.Builder`

## 1.1.6

Generalized return type in `runGeneralTCPServer`.

## 1.1.5

Added `sinkParserEither` ([pull request #189](https://github.com/snoyberg/conduit/pull/189))
