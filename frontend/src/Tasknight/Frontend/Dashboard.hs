{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tasknight.Frontend.Dashboard (Dashboard, serverDashboard) where

import Servant.API    ((:>), Get, PlainText)
import Servant.Server (Server)

import Tasknight.Frontend.Auth

type Dashboard = "dashboard" :> Auth :> Get '[PlainText] String

serverDashboard :: Server Dashboard
serverDashboard _user = pure "Dashboard is here"
