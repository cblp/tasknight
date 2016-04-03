{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.OAuth2
    (OAuth2Provider(..), OAuth2Scope, OAuth2Token, TokenRequest(..), defaultOAuth2Provider) where

import Data.Monoid ((<>))
import Network.Google.OAuth2 (OAuth2Client(..), OAuth2Scope, OAuth2Token, getAccessToken)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

import Tasknight.Storage (Storage(..))

type UserId = FilePath

data TokenRequest = TokenRequest
    { clientRegisterPage  :: String
    , scopes              :: [OAuth2Scope]
    , serviceName         :: String
    , userId              :: UserId
    }

data OAuth2Provider = OAuth2Provider{getToken :: TokenRequest -> IO OAuth2Token}

defaultOAuth2Provider :: Storage String String  -- ^ config storage
                      -> Storage UserId OAuth2Token  -- ^ token cache
                      -> OAuth2Provider
defaultOAuth2Provider Storage{getValue=getConfigValue, getStorageLocation=getConfigLocation}
                      Storage{getStorageLocation=getCacheLocation} =
    OAuth2Provider{getToken}
  where
    getToken TokenRequest{clientRegisterPage, scopes, serviceName, userId} = do
        let keyClientId = serviceName </> "clientId"
            keyClientSecret = serviceName </> "clientSecret"
        mClientId <- getConfigValue keyClientId
        mClientSecret <- getConfigValue keyClientSecret
        (clientId, clientSecret) <- case (mClientId, mClientSecret) of
            (Just clientId, Just clientSecret) -> pure (clientId, clientSecret)
            _ -> do
                clientIdLocation <- getConfigLocation keyClientId
                clientSecretLocation <- getConfigLocation keyClientSecret
                fail $ unlines
                    [ "Please register an application at"
                    , "  " <> clientRegisterPage
                    , "and put its id and secret to:"
                    , "  - " <> clientIdLocation
                    , "  - " <> clientSecretLocation
                    ]
        let client = OAuth2Client{clientId, clientSecret}
        tokenCacheFile <- getCacheLocation $ serviceName </> userId
        createDirectoryIfMissing True $ takeDirectory tokenCacheFile
        getAccessToken client scopes $ Just tokenCacheFile
