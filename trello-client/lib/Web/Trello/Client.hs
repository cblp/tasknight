{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Trello.Client
  ( runTrelloClient
  , getMyBoards
  ) where

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ServantError)

import Web.Trello.API.Types (Board)

import qualified Web.Trello.Client.Internal as I

newtype TrelloClient a =
    TrelloClient (ReaderT (Manager, BaseUrl) (ExceptT ServantError IO) a)
    deriving (Applicative, Functor, Monad, MonadIO)

runTrelloClient
    :: (MonadIO io)
    => Manager -> BaseUrl -> TrelloClient a -> ExceptT ServantError io a
runTrelloClient manager baseurl (TrelloClient areader) =
    mapExceptT liftIO $ runReaderT areader (manager, baseurl)

getMyBoards :: TrelloClient [Board]
getMyBoards =
    TrelloClient $ do
        (manager, baseurl) <- ask
        lift $ I.getMyBoards manager baseurl
