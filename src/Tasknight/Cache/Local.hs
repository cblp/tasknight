{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Cache.Local (localCache, localConfig) where

import System.Environment.XDG.BaseDir (getUserCacheFile, getUserConfigFile)

import Tasknight.Cache (Cache(..))

localCache :: String -> Cache FilePath String
localCache appName = Cache{getValue}
  where
    getValue key = do
        filePath <- getUserCacheFile appName key
        readFile filePath

localConfig :: String -> Cache FilePath String
localConfig appName = Cache{getValue}
  where
    getValue key = do
        filePath <- getUserConfigFile appName key
        readFile filePath
