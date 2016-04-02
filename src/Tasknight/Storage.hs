module Tasknight.Storage (Storage(..)) where

import Control.Monad.Trans.Maybe (MaybeT)

data Storage k v = Storage{getValue :: k -> MaybeT IO v, getStorageLocation :: k -> IO String}
