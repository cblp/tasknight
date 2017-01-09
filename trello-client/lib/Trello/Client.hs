{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trello.Client
    ( ClientEnv(..)
    , Key(..)
    , Token(..)
    , production
    , runTrelloClient
    -- * API Methods
    , getMyBoards
    ) where

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ServantError)

import Trello.API (Key(..), Token(..))
import Trello.API.Types (Board)

import           Trello.Client.BaseUrl (production)
import qualified Trello.Client.Internal as I

data ClientEnv = ClientEnv
    { manager :: Manager
    , baseurl :: BaseUrl
    , key :: Key
    , token :: Token
    }

newtype Client a =
    Client (ReaderT ClientEnv (ExceptT ServantError IO) a)
    deriving (Applicative, Functor, Monad, MonadIO)

runTrelloClient
    :: MonadIO io => ClientEnv -> Client a -> ExceptT ServantError io a
runTrelloClient clientEnv (Client areader) =
    mapExceptT liftIO $ runReaderT areader clientEnv

getMyBoards :: Client [Board]
getMyBoards =
    Client $ do
        ClientEnv{manager, baseurl, key, token} <- ask
        lift $ I.getMyBoards (Just key) (Just token) manager baseurl
