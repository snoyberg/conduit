import Network.Wai.Handler.DevelServer (runQuit)
import Data.Enumerator (Iteratee)
import Data.ByteString (ByteString)

main = runQuit 3000 "TestApp2" "testapp" (const $ return ["hamlet/testapp.hamlet"])
