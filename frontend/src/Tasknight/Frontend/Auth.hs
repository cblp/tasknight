{-# LANGUAGE DataKinds #-}

module Tasknight.Frontend.Auth (Auth, authCheck) where

import Servant.API    (BasicAuth, BasicAuthData(..))
import Servant.Server (BasicAuthCheck(..), BasicAuthResult(Unauthorized))

import Tasknight.Frontend.Types (User)

type Auth = BasicAuth "Tasknight" User

authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck $ \BasicAuthData{} -> pure Unauthorized
