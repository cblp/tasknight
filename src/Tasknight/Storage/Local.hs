{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Storage.Local (localCache, localConfig) where

import System.Environment.XDG.BaseDir (getUserCacheFile, getUserConfigFile)

import Tasknight.Storage (Storage(..))

localCache :: String -> Storage FilePath String
localCache = localStorage getUserCacheFile

localConfig :: String -> Storage FilePath String
localConfig = localStorage getUserConfigFile

localStorage :: (String -> String -> IO FilePath) -> String -> Storage FilePath String
localStorage getStorageFilePath appName = Storage{getValue}
  where
    getValue key = do
        filePath <- getStorageFilePath appName key
        readFile filePath
