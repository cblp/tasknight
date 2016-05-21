{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tasknight.Frontend where

import Data.Proxy     (Proxy(..))
import Network.Wai    (Application)
import Servant.API    ((:<|>)(..))
import Servant.Server (BasicAuthCheck(..), Context((:.), EmptyContext), Server, serveWithContext)

import Tasknight.Frontend.Auth      (authCheck)
import Tasknight.Frontend.Dashboard (Dashboard, serverDashboard)
import Tasknight.Frontend.Root      (Root, serverRoot)
import Tasknight.Frontend.Types     (User)

type Frontend = Dashboard :<|> Root

server :: Server Frontend
server = serverDashboard :<|> serverRoot

context :: Context '[BasicAuthCheck User]
context = authCheck :. EmptyContext

app :: Application
-- TODO(cblp, 2016-05-21) use VTA: Proxy @Frontend
app = serveWithContext (Proxy :: Proxy Frontend) context server
