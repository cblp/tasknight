{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.OAuth2
    (OAuth2Provider(..), OAuth2Scope, OAuth2Token, TokenRequest(..), defaultOAuth2Provider) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Monoid ((<>))
import Network.Google.OAuth2 (OAuth2Client(..), OAuth2Scope, OAuth2Token, getAccessToken)
import System.FilePath ((</>))

import Tasknight.Storage (Storage(..))

type TokenId = FilePath

data TokenRequest = TokenRequest{tokenId :: TokenId, scopes :: [OAuth2Scope], serviceName :: String}

data OAuth2Provider = OAuth2Provider{getToken :: TokenRequest -> IO OAuth2Token}

defaultOAuth2Provider :: Storage String String  -- ^ config storage
                      -> Storage TokenId OAuth2Token  -- ^ token cache
                      -> OAuth2Provider
defaultOAuth2Provider Storage{getValue=getConfigValue, getStorageLocation=getConfigLocation}
                      Storage{getValue=getCacheValue} =
    OAuth2Provider{getToken}
  where

    getMandatoryConfigValue (paramKey, paramDescription) = do
        mValue <- runMaybeT $ getConfigValue paramKey
        case mValue of
            Just value ->
                pure value
            Nothing -> do
                confLoc <- getConfigLocation paramKey
                fail $ "Please put " <> paramDescription <> " in " <> confLoc

    getToken TokenRequest{serviceName, tokenId, scopes} = do
        -- (key, description)
        let paramClientId = (serviceName </> "clientId", serviceName <> " client ID")
            paramClientSecret = (serviceName </> "clientSecret", serviceName <> " client secret")

        clientId <- getMandatoryConfigValue paramClientId
        clientSecret <- getMandatoryConfigValue paramClientSecret
        let client = OAuth2Client{clientId, clientSecret}
        mToken <- runMaybeT $ getCacheValue tokenId <|> lift (getAccessToken client scopes Nothing)
        -- case mtoken of
        --     Nothing ->
        --     Just token ->
        error "unimplemented defaultOAuth2Provider.getToken" mToken
