{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp

main = run 3000 $ const $ return $ ResponseFile status200 [("Content-Type", "text/plain"), ("Content-Length", "16")] "test.txt"
