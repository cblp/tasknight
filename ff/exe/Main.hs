{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Exception (Exception, throwIO)
import           Control.Monad (when, (>=>))
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, asks, runReaderT)
import qualified Data.ByteString as ByteString
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Options.Applicative (ParserInfo, command, execParser, fullDesc,
                                      help, helper, info, long, optional,
                                      progDesc, short, subparser, switch)
import           System.Environment.XDG.BaseDir (getUserCacheFile)
import           System.FilePath ((</>))
import           System.IO (hPutStrLn, stderr)

import Trello.Client (ClientEnv(..), Key(..), Token(..), getMyBoards,
                      production, runTrelloClient)

data Cmd
    = Now
    deriving (Show, Read)

data Options = Options
    { o_verbose :: Bool
    , o_cmd :: Cmd
    } deriving (Show)

programInfo :: ParserInfo Options
programInfo =
    info (helper <*> options) $
    fullDesc <> progDesc "Tasknight command-line interface"
  where
    options =
        Options <$>
        switch
            (long "verbose" <> short 'v' <>
             help "Write to stderr what's happening") <*>
        (fromMaybe Now <$>
         optional
             (subparser . command "now" . info nowOptions $
              progDesc "Show what you can do now (default)"))
    nowOptions = pure Now

main :: IO ()
main = do
    options@Options{o_cmd} <- execParser programInfo
    (`runReaderT` options) $ do
        logVerbose $ "options = " <> show options
        runCmd o_cmd

logVerbose
    :: (MonadReader Options io, MonadIO io)
    => String -> io ()
logVerbose msg = do
    v <- asks o_verbose
    when v . liftIO $ hPutStrLn stderr msg

runCmd
    :: MonadIO io
    => Cmd -> io ()
runCmd Now = do
    manager <- liftIO $ newManager tlsManagerSettings
    key <- loadKey
    token <- loadToken
    let clientEnv = ClientEnv{manager, baseurl=production, key, token}
    liftIO . giveup . runTrelloClient clientEnv $
        getMyBoards >>= liftIO . print

-- | Run 'EitherT' action in a dirty manner -- throwing error as exception.
giveup
    :: (MonadIO io, Exception e)
    => ExceptT e io a -> io a
giveup = runExceptT >=> either (liftIO . throwIO) pure

loadKey :: MonadIO io => io Key
loadKey = Key <$> readCacheItem "key"

loadToken :: MonadIO io => io Token
loadToken = Token <$> readCacheItem "token"

readCacheItem
    :: MonadIO io
    => FilePath -- file path relative to application cache directory
    -> io Text
readCacheItem name = do
    file <- getCacheFilePath name
    Text.strip <$> readFileUtf8 file
  where
    readFileUtf8 = liftIO . fmap decodeUtf8 . ByteString.readFile
    getCacheFilePath = liftIO . getUserCacheFile ("tasknight" </> "client")
