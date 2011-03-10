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
import Data.Enumerator (($$), enumList, Step (..), Enumerator, Iteratee, Enumeratee, joinI, checkDone, continue, Stream (..), (>>==))
import Blaze.ByteString.Builder (copyByteString, Builder)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Data.Monoid (mappend)
import qualified Data.Ascii as A
import Control.Monad (join)

-- | Wrap json responses in a jsonp callback.
--
-- Basically, if the user requested a \"text\/javascript\" and supplied a
-- \"callback\" GET parameter, ask the application for an
-- \"application/json\" response, then convern that into a JSONP response,
-- having a content type of \"text\/javascript\" and calling the specified
-- callback function.
jsonp :: Middleware
jsonp app env = do
    let accept = maybe B8.empty A.toByteString $ lookup "Accept" $ requestHeaders env
    let callback :: Maybe B8.ByteString
        callback =
            if B8.pack "text/javascript" `B8.isInfixOf` accept
                then join $ lookup "callback" $ queryString env
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
    go c r@(ResponseBuilder s hs b) =
        case checkJSON hs of
            Nothing -> return r
            Just hs' -> return $ ResponseBuilder s hs' $
                copyByteString c
                `mappend` fromChar '('
                `mappend` b
                `mappend` fromChar ')'
    go c (ResponseEnumerator e) = addCallback c e
    go' :: B8.ByteString -> Response -> [(A.CIAscii, A.Ascii)] -> Iteratee B8.ByteString IO Response
    go' c r hs =
        case checkJSON hs of
            Just _ -> addCallback c $ responseEnumerator r
            Nothing -> return r
    checkJSON :: [(A.CIAscii, A.Ascii)] -> Maybe [(A.CIAscii, A.Ascii)]
    checkJSON hs =
        case fmap A.toString $ lookup "Content-Type" hs of
            Just "application/json" -> Just $ fixHeaders hs
            _ -> Nothing
    fixHeaders = changeVal "Content-Type" "text/javascript"
    addCallback :: B8.ByteString -> (forall a. ResponseEnumerator a)
                -> Iteratee B8.ByteString IO Response
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
                 $$ enumList 1 [copyByteString cb, fromChar '('] step
        after :: Enumerator Builder IO b -> Enumeratee Builder Builder IO b
        after enum =
            loop
          where
            loop = checkDone $ continue . step
            step k EOF = enum (Continue k) >>== return
            step k s = k s >>== loop

changeVal :: Eq a
          => a
          -> A.Ascii
          -> [(a, A.Ascii)]
          -> [(a, A.Ascii)]
changeVal key val old = (key, val)
                      : filter (\(k, _) -> k /= key) old
