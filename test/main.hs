import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import           Data.Conduit.Cereal
import           Data.Conduit.List (sourceList)
import           Data.Serialize
import           Test.Hspec.Monadic
import           Test.Hspec.QuickCheck
import           Test.Hspec.HUnit
import           Test.QuickCheck hiding (property)
import           Test.HUnit

main = hspec $
    describe "Issues relaited" $ do
        it "(sourceList []) C.$$ (sinkGet (getWord8))" $ do
            result <- (sourceList []) C.$$ (sinkGet getWord8)
            AssertLeft result @? "Should return `Left' with message like:\ntoo few bytes\nFrom:\tdemandInput\n\n"

data AssertLeft a b = AssertLeft (Either a b)

instance AssertionPredicable (AssertLeft a b) where
    assertionPredicate (AssertLeft (Left _)) = return False
    assertionPredicate (AssertLeft (Right _)) = return False