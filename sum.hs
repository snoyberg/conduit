import Prelude hiding (take)
import Data.Conduit
import Data.Conduit.List
import Control.Monad.Trans.Resource

main :: IO ()
main = do
    runResourceT (fromList [1..10] $$ do
        _ <- take 5
        sum') >>= putStrLn

sum' :: Sink Int IO String
sum' = fmap show $ fold (+) 0
