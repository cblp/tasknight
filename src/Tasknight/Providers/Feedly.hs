module Tasknight.Providers.Feedly (Feedly(..), feedly) where

import Tasknight.Provider (Provider(..))

-- | Provider configuration
data Feedly = Feedly

-- | Provider constructor
feedly :: Feedly -> Provider
feedly Feedly = Provider
