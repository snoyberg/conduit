{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid
import Data.Enumerator (run_, enumList, ($$))

main = run 3000 app

app req = return $
    case pathInfo req of
        "/builder/withlen" -> builderWithLen
        "/builder/nolen" -> builderNoLen
        "/enum/withlen" -> enumWithLen
        "/enum/nolen" -> enumNoLen
        "/file/withlen" -> fileWithLen
        "/file/nolen" -> fileNoLen
        _ -> index $ pathInfo req

builderWithLen = ResponseBuilder
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    $ copyByteString "PONG"

builderNoLen = ResponseBuilder
    status200
    [ ("Content-Type", "text/plain")
    ]
    $ copyByteString "PONG"

fileWithLen = ResponseFile
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    "pong.txt"

fileNoLen = ResponseFile
    status200
    [ ("Content-Type", "text/plain")
    ]
    "pong.txt"

enumWithLen = ResponseEnumerator $ \f ->
    run_ $ (enumList 1 $ map copyByteString ["P", "O", "NG"]) $$ f
        status200
        [ ("Content-Type", "text/plain")
        , ("Content-Length", "4")
        ]

enumNoLen = ResponseEnumerator $ \f ->
    run_ $ (enumList 1 $ map copyByteString ["P", "O", "NG"]) $$ f
        status200
        [ ("Content-Type", "text/plain")
        ]

index p = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p><a href='/builder/withlen'>builder withlen</a></p>\n"
    , "<p><a href='/builder/nolen'>builder nolen</a></p>\n"
    , "<p><a href='/enum/withlen'>enum withlen</a></p>\n"
    , "<p><a href='/enum/nolen'>enum nolen</a></p>\n"
    , "<p><a href='/file/withlen'>file withlen</a></p>\n"
    , "<p><a href='/file/nolen'>file nolen</a></p>\n"
    , p
    ]
