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
