<!-- Generated from README.lhs, do not modify! -->
WAI: Web Application Interface
==============================

Getting started
---------------

You want a minimal example? Here it is!

    {-# LANGUAGE OverloadedStrings #-}
    import Network.Wai
    import Network.Wai.Enumerator (fromLBS)
    import Network.Wai.Handler.SimpleServer (run)

    app :: Application
    app _ = return Response
        { status          = status200
        , responseHeaders = [("Content-Type", "text/plain")]
        , responseBody    = ResponseLBS "Hello, Web!"
        }

    main :: IO ()
    main = do
        putStrLn $ "http://localhost:8080/"
        run 8080 app

Put that code into a file named _hello.hs_ and install [wai] and [wai-extra] from Hackage:

    cabal install wai wai-extra

Run it:

    runhaskell hello.hs

Point your browser to:

    http://localhost:8080/


Serving static content
----------------------

We can modify our previous example to serve static content. For this create a file named _index.html_:

    <p>Hello, Web!</p>

Now we redefine `responseBody` to refer to that file:

    app2 :: Application
    app2 _ = return index

    index = Response
        { status          = status200
        , responseHeaders = [("Content-Type", "text/html")]
        , responseBody    = ResponseFile "index.html"
        }


Basic dispatching
-----------------

An `Application` maps `Request`s to `Response`s:

    ghci> :info  Application
    type Application = Request -> IO Response

Depending on the path info provided with each `Request` we can serve different `Response`s:

    app3 :: Application
    app3 request = case pathInfo request of
        "/"     -> return index
        "/raw/" -> return plainIndex
        _       -> return notFound

    plainIndex = Response
        { status          = status200
        , responseHeaders = [("Content-Type", "text/plain")]
        , responseBody    = ResponseFile "index.html"
        }

    notFound = Response
        { status          = status404
        , responseHeaders = [("Content-Type", "text/plain")]
        , responseBody    = ResponseLBS "404 - Not Found"
        }


Doing without overloaded strings
--------------------------------

For the sake of efficiency, WAI uses the [bytestring] package.  We used GHCs [overloaded strings] to almost hide this fact. But we can easily do without.  What follows is a more verbose definition of `notFound`, that works without GHC extensions:

    import qualified Data.ByteString.Char8 as B8
    import qualified Data.ByteString.Lazy.Char8 as LB8

    notFound = Response
        { status          = Status404
        , responseHeaders = [("Content-Type", B8.pack "text/plain")]
        , responseBody    = Right $ fromLBS $ LB8.pack "404 - Not Found"
        }


 [wai]: http://hackage.haskell.org/package/wai
 [wai-extra]: http://hackage.haskell.org/package/wai-extra
 [overloaded strings]: http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#overloaded-strings
 [bytestring]: http://hackage.haskell.org/package/bytestring
