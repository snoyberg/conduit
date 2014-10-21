{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Blaze
import Criterion.Main
import Blaze.ByteString.Builder
import Data.Monoid
import qualified Data.ByteString.Builder as BS
import Data.Functor.Identity (runIdentity)
import Control.Monad.ST (runST)
import Data.ByteString.Lazy.Internal (defaultChunkSize)

count :: Int
count = 100000

single :: Builder
single = copyByteString "Hello World!\n"

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

singleBS :: BS.Builder
singleBS = BS.shortByteString "Hello World!\n"

oneBSBuilderLeft :: BS.Builder
oneBSBuilderLeft =
    loop count mempty
  where
    loop 0 b = b
    loop i b = loop (i - 1) (b <> singleBS)

oneBSBuilderRight :: BS.Builder
oneBSBuilderRight =
    loop count mempty
  where
    loop 0 b = b
    loop i b = loop (i - 1) (b <> singleBS)

builderBSSource :: Monad m => Source m BS.Builder
builderBSSource = CL.replicate count singleBS

main :: IO ()
main = defaultMain
    [ bench "conduit, strict, safe" $ whnfIO $
        builderSource $$ builderToByteString =$ CL.sinkNull
    , bench "conduit, strict, unsafe" $ whnfIO $
        builderSource $$ unsafeBuilderToByteString (allocBuffer defaultChunkSize) =$ CL.sinkNull

    , bench "one builder, left" $ nf toLazyByteString oneBuilderLeft
    , bench "one builder, right" $ nf toLazyByteString oneBuilderRight
    , bench "conduit, lazy" $ flip nf builderSource $ \src ->
        toLazyByteString $ runIdentity $ src $$ CL.fold (<>) mempty

    , bench "one bs builder, left" $ nf BS.toLazyByteString oneBSBuilderLeft
    , bench "one bs builder, right" $ nf BS.toLazyByteString oneBSBuilderRight
    , bench "conduit BS, lazy" $ flip nf builderBSSource $ \src ->
        BS.toLazyByteString $ runIdentity $ src $$ CL.fold (<>) mempty
    ]
