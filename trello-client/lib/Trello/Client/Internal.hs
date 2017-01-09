module Trello.Client.Internal
    ( getMyBoards
    ) where

import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ClientM, client)

import Trello.API (Key, Token, api)
import Trello.API.Types (Board)

type RawClient a =
    Maybe Key -> Maybe Token -> Manager -> BaseUrl -> ClientM [Board]

getMyBoards :: RawClient [Board]
getMyBoards = client api
