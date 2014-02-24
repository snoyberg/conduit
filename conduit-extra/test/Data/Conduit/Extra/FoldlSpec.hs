module Data.Conduit.Extra.FoldlSpec (spec) where
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Functor.Identity (runIdentity)
import Data.Conduit.Extra
import Data.Conduit
import qualified Control.Foldl as F
import Control.Applicative ((<$>), (<*>))

spec :: Spec
spec = describe "Data.Conduit.Extra.Foldl" $ do
    prop "sum" $ \nums ->
        let res = runIdentity $ mapM_ yield nums $$ sinkFold F.sum
         in res `shouldBe` (sum nums :: Int)
    prop "average" $ \num nums' ->
        let nums = num:nums' -- avoid divide by zero
            average = (/) <$> F.sum <*> F.genericLength
            res = runIdentity $ mapM_ yield nums $$ sinkFold average
         in res `shouldBe` (sum nums / fromIntegral (length nums) :: Double)
