{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.OAuth2
    (OAuth2Provider(..), OAuth2Scope, OAuth2Token, TokenRequest(..), defaultOAuth2Provider) where

import Control.Applicative ((<|>))
import Network.Google.OAuth2 (OAuth2Client(..), OAuth2Scope, OAuth2Token, getAccessToken)

import Tasknight.Storage (Storage(..))

type TokenId = FilePath

data TokenRequest = TokenRequest{tokenId :: TokenId, scopes :: [OAuth2Scope]}

data OAuth2Provider = OAuth2Provider{getToken :: TokenRequest -> IO OAuth2Token}

defaultOAuth2Provider :: Storage String String  -- ^ config storage
                      -> Storage TokenId OAuth2Token  -- ^ token cache
                      -> OAuth2Provider
defaultOAuth2Provider Storage{getValue=getConfigValue} Storage{getValue=getCacheValue} =
    OAuth2Provider{getToken}
  where
    getToken TokenRequest{tokenId, scopes} = do
        clientId <- getConfigValue "clientId"
        clientSecret <- getConfigValue "clientSecret"
        let client = OAuth2Client{clientId, clientSecret}
        mtoken <- getCacheValue tokenId <|> getAccessToken client scopes Nothing
        -- case mtoken of
        --     Nothing ->
        --     Just token ->
        error "unimplemented defaultOAuth2Provider.getToken" mtoken
