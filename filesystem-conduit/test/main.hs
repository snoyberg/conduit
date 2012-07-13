{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Test.Hspec
{-
import Test.Hspec.QuickCheck (prop)
import Test.HUnit

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Lazy as CLazy
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Data.Conduit (runResourceT)
import Control.Monad.ST (runST)
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.IORef as I
import Blaze.ByteString.Builder (fromByteString, toLazyByteString, insertLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Data.Maybe (catMaybes)
import Control.Monad.Trans.Writer (Writer)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Monad.Trans.Resource (runExceptionT_, withIO, resourceForkIO)
import Control.Concurrent (threadDelay, killThread)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (pure, (<$>), (<*>))
-}

main :: IO ()
main = hspec $ do
    return ()
