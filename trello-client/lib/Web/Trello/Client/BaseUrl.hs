module Web.Trello.Client.BaseUrl
  ( production
  ) where

import Servant.Client (BaseUrl(..), Scheme(Https))

production :: BaseUrl
production = BaseUrl Https "api.trello.com" 443 "/"
