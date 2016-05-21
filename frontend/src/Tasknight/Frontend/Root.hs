{-# LANGUAGE DataKinds #-}

-- | Root page just redirects to Dashboard
module Tasknight.Frontend.Root (Root, serverRoot) where

import Servant.API    (Header, Headers, PlainText, StdMethod(GET), Verb, addHeader)
import Servant.Server (Server)

type Root = Verb 'GET 303 '[PlainText] (Headers '[Header "Location" String] String)

serverRoot :: Server Root
serverRoot = pure $ addHeader "/dashboard" ""
