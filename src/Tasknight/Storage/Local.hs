{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Storage.Local (localCache, localConfig) where

import Control.Exception (Exception, catchJust)
import System.Environment.XDG.BaseDir (getUserCacheFile, getUserConfigFile)
import System.IO.Error (isDoesNotExistError)

import Tasknight.Storage (Storage(..))

localCache :: String -> Storage FilePath String
localCache = localStorage getUserCacheFile

localConfig :: String -> Storage FilePath String
localConfig = localStorage getUserConfigFile

localStorage :: (String -> String -> IO FilePath) -> String -> Storage FilePath String
localStorage getStorageFilePath appName = Storage{getValue, getStorageLocation}
  where
    getValue key = do
        filePath <- getStorageLocation key
        catchMaybe isDoesNotExistError (readFile filePath)
    getStorageLocation = getStorageFilePath appName

catchMaybe  :: Exception e
            => (e -> Bool)  -- ^ predicate
            -> IO a         -- ^ body
            -> IO (Maybe a)
catchMaybe p body =
    catchJust
        (\ex -> if p ex then Just () else Nothing)
        (Just <$> body)
        (\() -> pure Nothing)
