## 1.2.0

* Drop `monad-control` and `mmorph` dependencies

## 1.1.11

* `runResourceTChecked`, which checks if any of the cleanup actions
  threw exceptions and, if so, rethrows them. __NOTE__ This is
  probably a much better choice of function than `runResourceT`, and
  in the next major version release, will become the new behavior of
  `runResourceT`.

## 1.1.10

* Added `MonadUnliftIO` instances and `UnliftIO.Resource`

## 1.1.9

* Add generalized version of resourceForkIO

## 1.1.8.1

* Allocation actions should be masked

## 1.1.8

* Add `instance MonadFix ResourceT`
  [#281](https://github.com/snoyberg/conduit/pull/281)

## 1.1.7.5

* Inline the tutorial from SoH

## 1.1.7.4

* Make test suite slightly more robust

## 1.1.7.3

* Doc tweak

## 1.1.7.2

* Remove upper bound on transformers [#249](https://github.com/snoyberg/conduit/issues/249)

## 1.1.7.1

* transformers-compat 0.5

## 1.1.7

* Canonicalise Monad instances [#237](https://github.com/snoyberg/conduit/pull/237)

## 1.1.6

* Safe/Trustworthy for resourcet [#220](https://github.com/snoyberg/conduit/pull/220)

## 1.1.5

*  Add pass-through instances for Alternative and MonadPlus [#214](https://github.com/snoyberg/conduit/pull/214)

## 1.1.4.1

* Allow older `exceptions` version again

## 1.1.4

* Add `MonadResource ExceptT` instance [#198](https://github.com/snoyberg/conduit/pull/198)

## 1.1.3.2

monad-control-1.0 support [#191](https://github.com/snoyberg/conduit/pull/191)

## 1.1.3

Provide the `withEx` function to interact nicely with the exceptions package.
