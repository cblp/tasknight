{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Trello.API
    ( API
    , Key
    , Token
    , api
    ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Servant.API ((:>), Get, JSON, QueryParam, ToHttpApiData)

import Trello.API.Types (Board)

-- | Application key
newtype Key = Key Text
    deriving (ToHttpApiData)

-- | Authorization token
newtype Token = Token Text
    deriving (ToHttpApiData)

type API =
    "1"
        :> "members"
            :> "me"
                :> "boards"
                    :> QueryParam "key" Key
                    :> QueryParam "token" Token
                    :> Get '[JSON] [Board]

api :: Proxy API
api = Proxy
