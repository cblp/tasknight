{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.OAuth2
    (OAuth2Provider(..), OAuth2Scope, OAuth2Token, TokenRequest(..), defaultOAuth2Provider) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Monoid ((<>))
import Network.Google.OAuth2 (OAuth2Client(..), OAuth2Scope, OAuth2Token, getAccessToken)

import Tasknight.Storage (Storage(..))

type TokenId = FilePath

data TokenRequest = TokenRequest{tokenId :: TokenId, scopes :: [OAuth2Scope]}

data OAuth2Provider = OAuth2Provider{getToken :: TokenRequest -> IO OAuth2Token}

defaultOAuth2Provider :: Storage String String  -- ^ config storage
                      -> Storage TokenId OAuth2Token  -- ^ token cache
                      -> OAuth2Provider
defaultOAuth2Provider Storage{getValue=getConfigValue, getStorageLocation=getConfigLocation}
                      Storage{getValue=getCacheValue} =
    OAuth2Provider{getToken}
  where

    keyClientId = "clientId"
    keyClientSecret = "clientSecret"

    getToken TokenRequest{tokenId, scopes} = do
        mClientId <- runMaybeT (getConfigValue keyClientId)
        clientId <- case mClientId of
            Just clientId ->
                pure clientId
            Nothing -> do
                confLoc <- getConfigLocation keyClientId
                fail $ "Please specify clientId in " <> confLoc
        mClientSecret <- runMaybeT (getConfigValue keyClientSecret)
        clientSecret <- case mClientSecret of
            Just clientSecret ->
                pure clientSecret
            Nothing -> do
                confLoc <- getConfigLocation keyClientSecret
                fail $ "Please specify clientSecret in " <> confLoc
        let client = OAuth2Client{clientId, clientSecret}
        mToken <- runMaybeT $ getCacheValue tokenId <|> lift (getAccessToken client scopes Nothing)
        -- case mtoken of
        --     Nothing ->
        --     Just token ->
        error "unimplemented defaultOAuth2Provider.getToken" mToken
