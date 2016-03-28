module Tasknight.Cache (Cache(..)) where

data Cache k v = Cache {getValue :: k -> IO v}
