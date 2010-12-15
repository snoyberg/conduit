{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Module        : Network.Wai.Middleware.Jsonp
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Automatic wrapping of JSON responses to convert into JSONP.
--
---------------------------------------------------------
module Network.Wai.Middleware.Jsonp (jsonp) where

import Network.Wai
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe)

takeCallback :: B8.ByteString -> Maybe B8.ByteString
takeCallback bs | B8.null bs = Nothing
takeCallback bs =
    let (x, y) = B8.break (== '=') bs
        (y', z) = B8.break (== '&') $ B8.drop 1 y
     in if x == B8.pack "callback"
            then Just y'
            else takeCallback $ B8.drop 1 z

dropQM :: B8.ByteString -> B8.ByteString
dropQM bs
    | B8.null bs = bs
    | B8.head bs == '?' = B8.tail bs
    | otherwise = bs

-- | Wrap json responses in a jsonp callback.
--
-- Basically, if the user requested a \"text\/javascript\" and supplied a
-- \"callback\" GET parameter, ask the application for an
-- \"application/json\" response, then convern that into a JSONP response,
-- having a content type of \"text\/javascript\" and calling the specified
-- callback function.
jsonp :: Middleware a
jsonp app env = do
    let accept = fromMaybe B8.empty $ lookup "Accept" $ requestHeaders env
    let callback :: Maybe B8.ByteString
        callback =
            if B8.pack "text/javascript" `B8.isInfixOf` accept
                then takeCallback $ dropQM $ queryString env
                else Nothing
    let env' =
            case callback of
                Nothing -> env
                Just _ -> env
                        { requestHeaders = changeVal "Accept"
                                           "application/json"
                                           $ requestHeaders env
                        }
    res <- app env'
    case callback of
        Nothing -> return res
        Just c -> go c res
  where
    go c r@(ResponseLBS _ hs _) = go' c r hs
    go c r@(ResponseFile _ hs _) = go' c r hs
    go c (ResponseEnumerator e) = go'' c e
    go' c r hs =
        case checkJSON hs of
            Just _ -> go'' c $ enumResponse r
            Nothing -> return r
    go'' e = undefined
    checkJSON hs =
        case fmap B8.unpack $ lookup "Content-Type" hs of
            Just "application/json" -> Just $ fixHeaders hs
            Nothing -> Nothing
    fixHeaders = changeVal "Content-Type" "text/javascript"

enumResponse :: Response a -> ResponseEnumerator a
enumResponse = error "FIXME"

{-
addCallback :: B8.ByteString -> Enumerator -> Enumerator
addCallback cb (Enumerator e) = Enumerator $ \iter a -> do
    ea' <- iter a $ B8.snoc cb '('
    case ea' of
        Left a' -> return $ Left a'
        Right a' -> do
            ea'' <- e iter a'
            case ea'' of
                Left a'' -> return $ Left a''
                Right a'' -> iter a'' $ B8.singleton ')'
-}

changeVal :: Eq a
          => a
          -> String
          -> [(a, B8.ByteString)]
          -> [(a, B8.ByteString)]
changeVal key val old = (key, B8.pack val)
                      : filter (\(k, _) -> k /= key) old
