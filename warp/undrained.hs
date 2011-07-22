{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder (fromByteString)

main = run 3000 $ const $ return $ responseBuilder
    status200
    [("Content-Type", "text/html")]
    $ fromByteString "<form method='post' enctype='multipart/form-data'><textarea name='foo'></textarea><input type='submit'></form>"
