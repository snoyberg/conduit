import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Data.Monoid (mappend)

main :: IO ()
main = do
    runResourceT (
      (CL.fromList [1..5] `mappend` CL.fromList [6..10])
        $= CL.concatMap (replicate 2)
        $$ do
            _ <- CL.take 10
            CL.map (* 2) =$ sum') >>= putStrLn

sum' :: Sink Int IO String
sum' = fmap show $ CL.fold (+) 0
