{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
import Network.Wai.Handler.FastCGI
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|/ Home GET|]
instance Yesod HelloWorld where approot _ = ""
getHome = return $ RepPlain $ cs "Hello World!"
main = toWaiApp HelloWorld >>= run
