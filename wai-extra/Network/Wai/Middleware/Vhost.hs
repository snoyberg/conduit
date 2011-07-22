module Network.Wai.Middleware.Vhost (vhost) where

import Network.Wai

vhost :: [(Request -> Bool, Application)] -> Application -> Application
vhost vhosts def req =
    case filter (\(b, _) -> b req) vhosts of
        [] -> def req
        (_, app):_ -> app req
