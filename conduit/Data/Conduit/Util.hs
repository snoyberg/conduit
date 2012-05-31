-- | Utility functions from older versions of @conduit@. These should be
-- considered deprecated, as there are now easier ways to handle their use
-- cases. This module is provided solely for backwards compatibility.
module Data.Conduit.Util
    ( -- * Source
      module Data.Conduit.Util.Source
      -- * Sink
    , module Data.Conduit.Util.Sink
      -- * Conduit
    , module Data.Conduit.Util.Conduit
    ) where

import Data.Conduit.Util.Source
import Data.Conduit.Util.Sink
import Data.Conduit.Util.Conduit
