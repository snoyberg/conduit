-- FIXME add back the ConduitM type for better error messages
-- FIXME deprecate mapInput, mapOutput
{-# OPTIONS_HADDOCK not-home #-}
module Data.Conduit.Internal
    ( module X
    ) where
    -- FIXME ConduitM (..)

import Data.Conduit.Internal.Composition as X
import Data.Conduit.Internal.Deprecated as X
import Data.Conduit.Internal.Primitives as X
import Data.Conduit.Internal.Pipe as X
