{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Arbitrary

import Data.Conduit
import Data.Conduit.Base64
import qualified Data.Conduit.List as CL

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import Data.Functor.Identity (runIdentity)

instance Arbitrary S.ByteString where
    arbitrary = fmap S8.pack arbitrary

main :: IO ()
main = hspec $ do
    describe "encode/decode is idempotent" $ do
        prop "non-url" $ \bss -> L.fromChunks bss == L.fromChunks (runIdentity (CL.sourceList bss $$ encode =$ decode =$ CL.consume))
        prop "url" $ \bss -> L.fromChunks bss == L.fromChunks (runIdentity (CL.sourceList bss $$ encodeURL =$ decodeURL =$ CL.consume))
    describe "encode is identical" $ do
        prop "non-url" $ \bss -> B64.encode (S.concat bss) == S.concat (runIdentity $ CL.sourceList bss $$ encode =$ CL.consume)
        prop "url" $ \bss -> B64U.encode (S.concat bss) == S.concat (runIdentity $ CL.sourceList bss $$ encodeURL =$ CL.consume)
