module Web.Trello.Client
  ( getMyBoards
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (lift)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ServantError)

import Web.Trello.API.Types (Board)

import qualified Web.Trello.Client.Internal as I

newtype TrelloClient a =
    TrelloClient (ReaderT (Manager, BaseUrl) (ExceptT ServantError IO) a)

getMyBoards :: TrelloClient [Board]
getMyBoards =
    TrelloClient $ do
        (mgr, base) <- ask
        lift $ I.getMyBoards mgr base
