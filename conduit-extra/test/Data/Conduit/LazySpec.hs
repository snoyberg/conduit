module Data.Conduit.LazySpec (spec) where

import qualified Data.Conduit.Lazy as CLazy
import Test.Hspec
import Control.Monad.IO.Class
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource
import Data.Monoid
import qualified Data.IORef as I
import Control.Monad (forever)

spec :: Spec
spec = describe "Data.Conduit.Lazy" $ do

    describe "lazy" $ do
        it' "works inside a ResourceT" $ runResourceT $ do
            counter <- liftIO $ I.newIORef 0
            let incr i = do
                    istate <- liftIO $ I.newIORef $ Just (i :: Int)
                    let loop = do
                            res <- liftIO $ I.atomicModifyIORef istate ((,) Nothing)
                            case res of
                                Nothing -> return ()
                                Just x -> do
                                    count <- liftIO $ I.atomicModifyIORef counter
                                        (\j -> (j + 1, j + 1))
                                    liftIO $ count `shouldBe` i
                                    C.yield x
                                    loop
                    loop
            nums <- CLazy.lazyConsume $ mconcat $ map incr [1..10]
            liftIO $ nums `shouldBe` [1..10]

        it' "returns nothing outside ResourceT" $ do
            bss <- runResourceT $ CLazy.lazyConsume $ CB.sourceFile "test/main.hs"
            bss `shouldBe` []

        it' "works with pure sources" $ do
            nums <- CLazy.lazyConsume $ forever $ C.yield 1
            take 100 nums `shouldBe` replicate 100 (1 :: Int)

it' :: String -> IO () -> Spec
it' = it
