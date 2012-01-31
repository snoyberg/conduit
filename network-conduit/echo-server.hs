import Data.Conduit
import Data.Conduit.Network

main :: IO ()
main = runTCPServer (ServerSettings 5000 Nothing) echo

echo :: Application
echo src sink = src $$ sink
