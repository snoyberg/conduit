{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder (fromByteString)

main = run 3000 $ const $ return $ responseBuilder
    status200
    [("Content-Type", "text/plain")]
    $ fromByteString "PONG"
