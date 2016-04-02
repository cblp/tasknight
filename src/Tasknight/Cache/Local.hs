{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Cache.Local (localCache) where

import System.Environment.XDG.BaseDir (getUserCacheFile)

import Tasknight.Cache (Cache(..))

localCache :: String -> Cache FilePath String
localCache appName = Cache{getValue}
  where
    getValue key = do
        filePath <- getUserCacheFile appName key
        readFile filePath
