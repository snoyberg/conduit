{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad     (replicateM_)
import           Data.ByteString   (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Text (decode, utf8)

src :: Source IO ByteString
src = replicateM_ 1000000 $ yield "Hello World!\n"

main :: IO ()
main = src $$ decode utf8 =$ CL.sinkNull
