{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Dropbox (Config(..), withDropbox, upload) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy   (ByteString)
import Data.Text              (Text)
import Network.HTTP.Simple    (httpLbs, parseRequest)

data Config = Config

newtype DropboxT m a = DropboxT (m a)
    deriving (Applicative, Functor, Monad)

withDropbox :: Monad m => Config -> DropboxT m a -> m a
withDropbox config (DropboxT action) = do
    connect config
    r <- action
    disconnect
    pure r

-- | Your intent when writing a file to some path.
-- This is used to determine what constitutes a conflict and what the autorename strategy is.
-- In some situations, the conflict behavior is identical:
-- (a) If the target path doesn't contain anything, the file is always written; no conflict.
-- (b) If the target path contains a folder, it's always a conflict.
-- (c) If the target path contains a file with identical contents, nothing gets written;
-- no conflict.
-- The conflict checking differs in the case where there's a file at the target path with contents
-- different from the contents you're trying to write.
data WriteMode
    = Add
      -- ^ Never overwrite the existing file.
      -- The autorename strategy is to append a number to the file name.
      -- For example, "document.txt" might become "document (2).txt".
    | Overwrite
      -- ^ Always overwrite the existing file.
      -- The autorename strategy is the same as it is for add.
    | Update Text
      -- ^ Overwrite if the given "rev" matches the existing file's "rev".
      -- The autorename strategy is to append the string "conflicted copy" to the file name.
      -- For example, "document.txt" might become "document (conflicted copy).txt"
      -- or "document (Panda's conflicted copy).txt".

data CommitInfo = CommitInfo
    { ci_path           :: FilePath
      -- ^ Path in the user's Dropbox to save the file.
    , ci_mode           :: WriteMode
      -- ^ Selects what to do if the file already exists. The default for this union is add.
    , ci_autorename     :: Bool
      -- ^ If there's a conflict, as determined by mode,
      -- have the Dropbox server try to autorename the file to avoid conflict.
      -- The default for this field is False.
    , ci_clientModified :: Maybe UTCTime
      -- ^ The value to store as the client_modified timestamp.
      -- Dropbox automatically records the time at which the file was written
      -- to the Dropbox servers.
      -- It can also record an additional timestamp, provided by Dropbox desktop clients,
      -- mobile clients, and API apps of when the file was actually created or modified.
    , ci_mute           :: Bool
      -- ^ Normally, users are made aware of any file modifications in their Dropbox account
      -- via notifications in the client software.
      -- If true, this tells the clients that this modification shouldn't result
      -- in a user notification.
      -- The default for this field is False.
    }

-- | Create a new file with the contents provided in the request.
-- Do not use this to upload a file larger than 150 MB.
-- Instead, create an upload session with 'withUploadSession'.
upload :: MonadIO io => FilePath -> ByteString -> DropboxT io ()
upload path body = DropboxT $ do
    let arg = CommitInfo{}
    req1 <- liftIO $ parseRequest req0
    let req = req1
            & addRequestHeader
                  "Authorization"
                  "Bearer _7m6KZrhdCIAAAAAAAALE54vWIWkUfCAoevDcAtJXHSSfFSujqDRMLU0IBc1IWAB"
            & addRequestHeader
                  "Dropbox-API-Arg"
                  "{\"path\": \"/Homework/math/Matrices.txt\",\"mode\": \"add\",\"autorename\": true,\"mute\": false}"
            & addRequestHeader "Content-Type" "application/octet-stream"
    resp <- httpLbs req
    pure ()
  where
    -- TODO(cblp, 2016-07-05) parseRequest_
    req0 = "POST https://content.dropboxapi.com/2/files/upload"

connect :: Config -> m ()
connect = error "connect"

disconnect :: m ()
disconnect = error "disconnect"
