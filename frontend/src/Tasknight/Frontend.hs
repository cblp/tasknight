{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tasknight.Frontend where

import Servant.API ((:<|>), (:>), BasicAuth, Get, Header, Headers, PlainText, StdMethod(GET), Verb)

data User = User { name :: String }

type Auth = BasicAuth "Tasknight" User

type Root = Verb 'GET 303 '[PlainText] (Headers '[Header "Location" String] String)

type Dashboard = Auth :> "dashboard" :> Get '[PlainText] String

type Frontend = Dashboard :<|> Root
