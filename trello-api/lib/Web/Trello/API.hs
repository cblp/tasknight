{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Trello.API
  ( API
  , api
  ) where

import Data.Proxy (Proxy(..))
import Servant.API ((:>), Get, JSON)

import Web.Trello.API.Types (Board)

type API = "1" :> "members" :> "me" :> "boards" :> Get '[JSON] [Board]

api :: Proxy API
api = Proxy
