{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Exception (throwIO, Exception)
import Control.Monad (when, (>=>))
import Control.Monad.Except (runExceptT, ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
       (execParser, info, helper, fullDesc, long, short, switch,
        ParserInfo, help, progDesc, subparser, command, optional)
import System.IO (hPutStrLn, stderr)

import Web.Trello.Client (runTrelloClient, getMyBoards)
import Web.Trello.Client.BaseUrl (production)

data Cmd =
    Now
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
    options@Options {o_cmd} <- execParser programInfo
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
    liftIO . giveup . runTrelloClient manager production $
        getMyBoards >>= liftIO . print

giveup
    :: (MonadIO io, Exception e)
    => ExceptT e io a -> io a
giveup = runExceptT >=> either (liftIO . throwIO) pure
