import Data.Conduit
import Data.Conduit.List
import Control.Monad.Trans.Resource

main :: IO ()
main = runResourceT (fromList [1..10] `connect` sum') >>= putStrLn

sum' :: Sink Int IO String
sum' = fmap show $ fold (+) 0
