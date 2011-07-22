import Network.Wai
import Data.Enumerator.List (consume)
import Blaze.ByteString.Builder (toByteString)
import Data.Monoid (mconcat)
import Data.ByteString.Char8 (pack)

main = do
    b <- responseEnumerator (ResponseFile undefined undefined "test.hs" (Just $ FilePart 7 7)) (const $ const consume)
    print $ toByteString (mconcat b) == pack "Network"
