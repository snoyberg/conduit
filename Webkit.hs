{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES webkit.c #-}
module Webkit(startBrowser) where

foreign import ccall "webkit.h start_browser"
    startBrowser :: IO ()
