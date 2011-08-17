import Network.Wai.Handler.DevelServer (runQuit)
import Data.Enumerator (Iteratee)
import Data.ByteString (ByteString)

main = runQuit 3000 "FullApp" "fullApp" (const $ return ["hamlet/testapp.hamlet"])
