import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Data.Monoid (mappend)
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Trans.Control
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    -- unbuffered, aka non-resumable
    runResourceT (
      (CL.sourceList [1..5] `mappend` CL.sourceList [6..10])
        C.$= CL.concatMap (replicate 2)
        C.$$ do
            _ <- CL.take 10
            CL.map (* 2) C.=$ sum') >>= putStrLn
    -- buffered, aka resumable, can be passed to another thread
    runResourceT $ do
        manswer <- newEmptyMVar
        bsrc <- C.bufferSource (CL.sourceList [1..10] C.$= CL.map (+ 2))
        fork $ do
            answer <- bsrc C.$$ sum' :: ResourceT IO String
            putMVar manswer answer
        answer <- takeMVar manswer
        liftIO $ putStrLn answer

sum' :: C.Sink Int IO String
sum' = fmap show $ CL.fold (+) 0
