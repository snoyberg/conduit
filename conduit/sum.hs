import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Data.Monoid (mappend)
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Trans.Control

main :: IO ()
main = do
    -- unbuffered, aka non-resumable
    runResourceT (
      (CL.fromList [1..5] `mappend` CL.fromList [6..10])
        <$=> CL.concatMap (replicate 2)
        <$$> do
            _ <- CL.take 10
            CL.map (* 2) <=$> sum') >>= putStrLn
    -- buffered, aka resumable, can be passed to another thread
    runResourceT $ do
        manswer <- newEmptyMVar
        bsrc <- bsourceM (CL.fromList [1..10] <$=> CL.map (+ 2))
        fork $ do
            answer <- bsrc $$ sum' :: ResourceT IO String
            putMVar manswer answer
        answer <- takeMVar manswer
        liftBase $ putStrLn answer

sum' :: SinkM Int IO String
sum' = fmap show $ CL.fold (+) 0
