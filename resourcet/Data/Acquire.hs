{-# LANGUAGE CPP #-}
-- | This was previously known as the Resource monad. However, that term is
-- confusing next to the ResourceT transformer, so it has been renamed.

module Data.Acquire
    ( Acquire
-- * Example usage of 'Acquire' for allocating a resource and freeing it up.
--
-- | The code makes use of 'mkAcquire' to create an 'Acquire' and uses 'allocateAcquire' to allocate the resource and register an action to free up the resource.
--
-- === __Reproducible Stack code snippet__
--
-- > #!/usr/bin/env stack
-- > {- stack
-- >      --resolver lts-10.0
-- >      --install-ghc
-- >      runghc
-- >      --package resourcet
-- > -}
-- > 
-- > {-#LANGUAGE ScopedTypeVariables#-}
-- > 
-- > import Data.Acquire
-- > import Control.Monad.Trans.Resource
-- > import Control.Monad.IO.Class
-- > 
-- > main :: IO ()
-- > main = runResourceT $ do
-- >     let (ack :: Acquire Int) = mkAcquire (do
-- >                           putStrLn "Enter some number"
-- >                           readLn) (\i -> putStrLn $ "Freeing scarce resource: " ++ show i)
-- >     (releaseKey, resource) <- allocateAcquire ack
-- >     doSomethingDangerous resource
-- >     liftIO $ putStrLn $ "Going to release resource immediately: " ++ show resource
-- >     release releaseKey
-- >     somethingElse
-- > 
-- > doSomethingDangerous :: Int -> ResourceT IO ()
-- > doSomethingDangerous i =
-- >     liftIO $ putStrLn $ "5 divided by " ++ show i ++ " is " ++ show (5 `div` i)
-- > 
-- > somethingElse :: ResourceT IO ()    
-- > somethingElse = liftIO $ putStrLn
-- >     "This could take a long time, don't delay releasing the resource!"
--
-- Execution output:
--
-- > ~ $ stack code.hs
-- > Enter some number
-- > 3
-- > 5 divided by 3 is 1
-- > Going to release resource immediately: 3
-- > Freeing scarce resource: 3
-- > This could take a long time, don't delay releasing the resource!
-- >
-- > ~ $ stack code.hs
-- > Enter some number
-- > 0
-- > 5 divided by 0 is Freeing scarce resource: 0
-- > code.hs: divide by zero
--
    , with
    , withAcquire
    , mkAcquire
    , mkAcquireType
    , allocateAcquire
    , ReleaseType (..)
    ) where

import Control.Monad.Trans.Resource.Internal
import Data.Acquire.Internal
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO)
import qualified Control.Exception as E

-- | Allocate a resource and register an action with the @MonadResource@ to
-- free the resource.
--
-- @since 1.1.0
allocateAcquire :: MonadResource m => Acquire a -> m (ReleaseKey, a)
allocateAcquire = liftResourceT . allocateAcquireRIO

allocateAcquireRIO :: Acquire a -> ResourceT IO (ReleaseKey, a)
allocateAcquireRIO (Acquire f) = ResourceT $ \istate -> liftIO $ E.mask $ \restore -> do
    Allocated a free <- f restore
    key <- registerType istate free
    return (key, a)

-- | Longer name for 'with', in case @with@ is not obvious enough in context.
--
-- @since 1.2.0
withAcquire :: MonadUnliftIO m => Acquire a -> (a -> m b) -> m b
withAcquire = with
{-# INLINE withAcquire #-}
