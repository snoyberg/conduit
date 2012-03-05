{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.ImageSize
    ( sinkImageSize
    , Size (..)
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as S
import Data.ByteString.Char8 ()
import Data.ByteString.Lazy.Char8 ()
import Control.Applicative ((<$>), (<*>))

data Size = Size { width :: Int, height :: Int }
    deriving (Show, Eq, Ord, Read)

sinkImageSize :: Monad m => C.Sink S.ByteString m (Maybe Size)
sinkImageSize =
    C.Processing (pushHeader id) close
  where
    close = return Nothing
    pushHeader front bs'
        | S.length bs >= 11 && S.take 5 (S.drop 6 bs) ==
            S.pack [0x4A, 0x46, 0x49, 0x46, 0x00] =
                sinkPush jpg $ S.drop 4 bs
        | S.length bs >= 6 && S.take 6 bs `elem` gifs =
            sinkPush gif $ S.drop 6 bs
        | S.length bs >= 8 && S.take 8 bs == S.pack [137, 80, 78, 71, 13, 10, 26, 10] =
            sinkPush png $ S.drop 8 bs
        | S.length bs < 11 = C.Processing (pushHeader $ S.append bs) close
        | otherwise = C.Done (Just bs) Nothing
      where
        bs = front bs'

    gifs = ["GIF87a", "GIF89a"]
    gif = do
        b <- CB.take 4
        let go x y = fromIntegral x + (fromIntegral y) * 256
        return $ case L.unpack b of
            [w1, w2, h1, h2] -> Just $ Size (go w1 w2) (go h1 h2)
            _ -> Nothing

    png = do
        _ <- CB.take 4 -- FIXME drop
        hdr <- CB.take 4
        if hdr == "IHDR"
            then do
                mw <- getInt 4 0
                mh <- getInt 4 0
                return $ Size <$> mw <*> mh
            else return Nothing

    sinkPush (C.Processing push _) x = push x
    sinkPush _ _ = error "Data.Conduit.ImageSize.sinkPush"

    jpg = do
        mi <- getInt 2 0
        case mi of
            Nothing -> return Nothing
            Just i -> do
                _ <- CB.take $ i - 2
                jpgFrame

    jpgFrame = do
        mx <- CB.head
        case mx of
            Just 255 -> do
                my <- CB.head
                case my of
                    Just 0xC0 -> do
                        _ <- CB.take 3
                        h <- getInt 2 0
                        w <- getInt 2 0
                        return $ Size <$> w <*> h
                    Just _ -> jpg
                    Nothing -> return Nothing
            _ -> return Nothing

getInt :: (Monad m, Integral i)
       => Int
       -> i
       -> C.Sink S.ByteString m (Maybe i)
getInt 0 i = return $ Just i
getInt len i = do
    mx <- CB.head
    case mx of
        Nothing -> return Nothing
        Just x -> getInt (len - 1) (i * 256 + fromIntegral x)
