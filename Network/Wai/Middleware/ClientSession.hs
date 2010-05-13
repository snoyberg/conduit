module Network.Wai.Middleware.ClientSession
    ( clientsession
    -- * Generating keys
    , Word256
    , defaultKeyFile
    , getKey
    , getDefaultKey
    ) where

import Prelude hiding (exp)
import Network.Wai
import Web.Encodings
import Data.List (partition)
import Data.Maybe (fromMaybe, mapMaybe)
import Web.ClientSession
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)
import Data.Time.LocalTime () -- Show instance of UTCTime
import Data.Time.Format (formatTime) -- Read instance of UTCTime
import System.Locale (defaultTimeLocale)
import Control.Monad (guard)
import qualified Data.ByteString.Char8 as B
import Control.Arrow (first)

-- | Automatic encrypting and decrypting of client session data.
--
-- Using the clientsession package, this middleware handles automatic
-- encryption, decryption, checking, expiration and renewal of whichever
-- cookies you ask it to. For example, if you tell it to deal with the
-- cookie \"IDENTIFIER\", it will do the following:
--
-- * When you specify an \"IDENTIFIER\" value in your 'Response', it will
-- encrypt the value, along with the session expiration date and the
-- REMOTE_HOST of the user. It will then be set as a cookie on the client.
--
-- * When there is an incoming \"IDENTIFIER\" cookie from the user, it will
-- decrypt it and check both the expiration date and the REMOTE_HOST. If
-- everything matches up, it will set the \"IDENTIFIER\" value in
-- 'hackHeaders'.
--
-- * If the client sent an \"IDENTIFIER\" and the application does not set
-- a new value, this will reset the cookie to a new expiration date. This
-- way, you do not have sessions timing out every 20 minutes.
--
-- As far as security: clientsesion itself handles hashing and encrypting
-- the data to make sure that the user can neither see not tamper with it.
clientsession :: [B.ByteString] -- ^ list of cookies to intercept
              -> Word256 -- ^ encryption key
              -> Int -- ^ minutes to live
              -> ([(B.ByteString, B.ByteString)] -> Application)
              -> Request
              -> IO Response
clientsession cnames key minutesToLive app env = do
    let hs = requestHeaders env
        initCookiesRaw :: B.ByteString
        initCookiesRaw = fromMaybe B.empty $ lookup Cookie hs
        nonCookies :: [(RequestHeader, B.ByteString)]
        nonCookies = filter (\(x, _) -> x /= Cookie) hs
        initCookies :: [(B.ByteString, B.ByteString)]
        initCookies = parseCookies initCookiesRaw
        cookies, interceptCookies :: [(B.ByteString, B.ByteString)]
        (interceptCookies, cookies) = partition (\(x, _) -> x `elem` cnames)
                                      initCookies
        cookiesRaw, remoteHost' :: B.ByteString
        cookiesRaw = B.concat $ combineCookies cookies
        remoteHost' = remoteHost env
    print ("intercept", cnames, interceptCookies)
    now <- getCurrentTime
    let convertedCookies :: [(B.ByteString, B.ByteString)]
        convertedCookies =
            mapMaybe (decodeCookie key now remoteHost') interceptCookies
    let env' = env { requestHeaders =
                              (Cookie, cookiesRaw)
                            -- FIXME not sure how I feel about the next line
                            : filter (\(x, _) -> x == Cookie)
                                     (requestHeaders env)
                            ++ nonCookies
                   }
    res <- app convertedCookies env'
    let interceptHeaders, responseHeaders' :: [(ResponseHeader, B.ByteString)]
        (interceptHeaders, responseHeaders') =
            partition (\(x, _) -> responseHeaderToBS x `elem` cnames)
            $ responseHeaders res
        interceptHeaders' :: [(B.ByteString, B.ByteString)]
        interceptHeaders' = map (first responseHeaderToBS) interceptHeaders
    let timeToLive :: Int
        timeToLive = minutesToLive * 60
    let exp = fromIntegral timeToLive `addUTCTime` now
    let formattedExp = B.pack $ formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" exp
    let oldCookies :: [(B.ByteString, B.ByteString)]
        oldCookies = filter
                        (\(k, _) -> k `notElem` map fst interceptHeaders')
                        convertedCookies
    let newCookies = map (setCookie key exp formattedExp remoteHost') $
                     oldCookies ++ interceptHeaders'
    print ("cookies", oldCookies, newCookies, cookiesRaw)
    let res' = res { responseHeaders = newCookies ++ responseHeaders' }
    return res'

combineCookies :: [(B.ByteString, B.ByteString)] -> [B.ByteString]
combineCookies [] = []
combineCookies ((k, v):rest) = k : B.singleton '=' : v : B.pack "; "
                             : combineCookies rest
setCookie :: Word256
          -> UTCTime -- ^ expiration time
          -> B.ByteString -- ^ formatted expiration time
          -> B.ByteString -- ^ remote host
          -> (B.ByteString, B.ByteString)
          -> (ResponseHeader, B.ByteString)
setCookie key exp fexp rhost (cname, cval) =
    (SetCookie, B.concat
                    [ cname
                    , B.singleton '='
                    , B.pack $ encrypt key $ B.pack $ show $ ACookie exp rhost cval
                    , B.pack "; path=/; expires="
                    , fexp
                    ])

data ACookie = ACookie UTCTime B.ByteString B.ByteString
    deriving (Show, Read)

decodeCookie :: Word256 -- ^ key
             -> UTCTime -- ^ current time
             -> B.ByteString -- ^ remote host field
             -> (B.ByteString, B.ByteString) -- ^ cookie pair
             -> Maybe (B.ByteString, B.ByteString)
decodeCookie key now rhost (cname, encrypted) = do
    decrypted <- decrypt key $ B.unpack encrypted
    (ACookie exp rhost' val) <- mread $ B.unpack decrypted
    guard $ exp > now
    guard $ rhost' == rhost
    guard $ not $ B.null val
    return (cname, val)

mread :: (Monad m, Read a) => String -> m a
mread s =
    case reads s of
        [] -> fail $ "Reading of " ++ s ++ " failed"
        ((x, _):_) -> return x
