module Tasknight.Storage (Storage(..)) where

data Storage k v = Storage{getValue :: k -> IO (Maybe v), getStorageLocation :: k -> IO String}
