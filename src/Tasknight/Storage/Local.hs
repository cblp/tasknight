{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Storage.Local (localCache, localConfig) where

import Control.Exception (Exception, catchJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
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
        filePath <- liftIO $ getStorageLocation key
        catchMaybeT isDoesNotExistError (readFile filePath)
    getStorageLocation = getStorageFilePath appName

catchMaybeT :: Exception e
            => (e -> Bool)  -- ^ predicate
            -> IO a         -- ^ body
            -> MaybeT IO a
catchMaybeT p body = MaybeT $
    catchJust
        (\ex -> if p ex then Just () else Nothing)
        (Just <$> body)
        (\() -> pure Nothing)
