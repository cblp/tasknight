module Tasknight.Storage (Storage(..)) where

data Storage k v = Storage {getValue :: k -> IO v}
