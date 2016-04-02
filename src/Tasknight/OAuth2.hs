{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.OAuth2
    (OAuth2Provider(..), OAuth2Scope, OAuth2Token, TokenRequest(..), defaultOAuth2Provider) where

import Control.Applicative ((<|>))
import Network.Google.OAuth2 (OAuth2Client(..), OAuth2Scope, OAuth2Token, getAccessToken)

import Tasknight.Cache (Cache(..))

type TokenId = FilePath

data TokenRequest = TokenRequest{tokenId :: TokenId, scopes :: [OAuth2Scope]}

data OAuth2Provider = OAuth2Provider{getToken :: TokenRequest -> IO OAuth2Token}

defaultOAuth2Provider :: Cache String String  -- ^ config storage
                      -> Cache TokenId OAuth2Token  -- ^ token cache
                      -> OAuth2Provider
defaultOAuth2Provider Cache{getValue=getConfigValue} Cache{getValue=getCacheValue} =
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
