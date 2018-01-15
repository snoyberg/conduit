{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.ByteString.Builder
import Gauge.Main
import Data.Monoid
import Data.ByteString.Builder
import Data.Functor.Identity (runIdentity)
import Control.Monad.ST (runST)
import Data.ByteString.Lazy.Internal (defaultChunkSize)

count :: Int
count = 100000

single :: Builder
single = shortByteString "Hello World!\n"

oneBuilderLeft :: Builder
oneBuilderLeft =
    loop count mempty
  where
    loop 0 b = b
    loop i b = loop (i - 1) (b <> single)

oneBuilderRight :: Builder
oneBuilderRight =
    loop count mempty
  where
    loop 0 b = b
    loop i b = loop (i - 1) (b <> single)

builderSource :: Monad m => Source m Builder
builderSource = CL.replicate count single

oneBSBuilderLeft :: Builder
oneBSBuilderLeft =
    loop count mempty
  where
    loop 0 b = b
    loop i b = loop (i - 1) (b <> single)

oneBSBuilderRight :: Builder
oneBSBuilderRight =
    loop count mempty
  where
    loop 0 b = b
    loop i b = loop (i - 1) (b <> single)

builderBSSource :: Monad m => ConduitT i Builder m ()
builderBSSource = CL.replicate count single

main :: IO ()
main = defaultMain
    [ bench "conduit, strict, safe" $ whnfIO $
        builderSource $$ builderToByteString =$ CL.sinkNull
    , bench "conduit, strict, unsafe" $ whnfIO $
        builderSource $$ unsafeBuilderToByteString =$ CL.sinkNull

    , bench "one builder, left" $ nf toLazyByteString oneBuilderLeft
    , bench "one builder, right" $ nf toLazyByteString oneBuilderRight
    , bench "conduit, lazy" $ flip nf builderSource $ \src ->
        toLazyByteString $ runIdentity $ src $$ CL.fold (<>) mempty

    , bench "one bs builder, left" $ nf toLazyByteString oneBSBuilderLeft
    , bench "one bs builder, right" $ nf toLazyByteString oneBSBuilderRight
    , bench "conduit BS, lazy" $ flip nf builderBSSource $ \src ->
        toLazyByteString $ runIdentity $ src $$ CL.fold (<>) mempty
    ]
