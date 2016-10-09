module Web.Trello.Client.Internal
  ( getMyBoards
  ) where

import Network.HTTP.Client (Manager)
import Servant.Client (client, ClientM, BaseUrl)

import Web.Trello.API (api)
import Web.Trello.API.Types (Board)

getMyBoards :: Manager -> BaseUrl -> ClientM [Board]
getMyBoards = client api
