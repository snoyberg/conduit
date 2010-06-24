{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES webkit.c #-}
module Webkit(startBrowser) where

foreign import ccall "start_browser"
    startBrowser :: IO ()
