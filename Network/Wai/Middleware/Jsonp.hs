{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Enumerator (($$), enumList, Step (..), Enumerator, Iteratee, Enumeratee, joinI, checkDone, continue, Stream (..), (>>==))
import Blaze.ByteString.Builder (fromByteString, Builder)
import Blaze.ByteString.Builder.Char8 (fromChar)

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
jsonp :: Middleware
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
    go c r@(ResponseFile _ hs _) = go' c r hs
    go c (ResponseEnumerator e) = addCallback c e
    go' c r hs =
        case checkJSON hs of
            Just _ -> addCallback c $ responseEnumerator r
            Nothing -> return r
    checkJSON hs =
        case fmap B8.unpack $ lookup "Content-Type" hs of
            Just "application/json" -> Just $ fixHeaders hs
            _ -> Nothing
    fixHeaders = changeVal "Content-Type" "text/javascript"
    addCallback :: B8.ByteString -> (forall a. ResponseEnumerator a) -> IO Response
    addCallback cb e =
        return $ ResponseEnumerator $ helper
      where
        helper f =
            e helper'
          where
            helper' s hs =
                case checkJSON hs of
                    Just hs' -> wrap $$ f s hs'
                    Nothing -> f s hs
        wrap :: Step Builder IO b -> Iteratee Builder IO b
        wrap step = joinI $ after (enumList 1 [fromChar ')'])
                 $$ enumList 1 [fromByteString cb, fromChar '('] step
        after :: Enumerator Builder IO b -> Enumeratee Builder Builder IO b
        after enum =
            loop
          where
            loop = checkDone $ continue . step
            step k EOF = enum (Continue k) >>== return
            step k s = k s >>== loop

changeVal :: Eq a
          => a
          -> String
          -> [(a, B8.ByteString)]
          -> [(a, B8.ByteString)]
changeVal key val old = (key, B8.pack val)
                      : filter (\(k, _) -> k /= key) old
