{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Servant                  ((:<|>)(..), BasicAuthCheck(..), BasicAuthData(..),
                                 BasicAuthResult(Unauthorized), Context((:.), EmptyContext),
                                 Proxy(..), Server, addHeader, serveWithContext)

import Tasknight.Frontend

authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck $ \BasicAuthData{} -> pure Unauthorized

context :: Context '[BasicAuthCheck User]
context = authCheck :. EmptyContext

serverRoot :: Server Root
serverRoot = pure (addHeader "/dashboard" "")

serverDashboard :: Server Dashboard
serverDashboard _user = pure "Dashboard is here"

server :: Server Frontend
server = serverDashboard :<|> serverRoot

app :: Application
app = serveWithContext (Proxy :: Proxy Frontend) context server

main :: IO ()
main = do
    putStrLn "Start"
    run 3000 app
