{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Providers.Gmail (Gmail(..), ListSpec(..), gmail, inboxUnread, starred) where

import            Control.Error (ExceptT(..), Script, fmapL, throwE)
import            Control.Monad.Trans.Class (lift)
import qualified  Data.ByteString.Base64 as Base64
import qualified  Data.ByteString.Char8 as ByteString
import            Data.Monoid ((<>))
import qualified  Data.Text as Text
import            ListT (ListT)
import            Network.Connection
import            Network.IMAP
import            Network.IMAP.Types

import Tasknight.OAuth2 (OAuth2Provider(..), OAuth2Scope, TokenRequest(..))
import Tasknight.Provider (Item(..), ItemList(..), Provider(..))

-- | List specificator
data ListSpec = ListSpec

inboxUnread :: ListSpec
inboxUnread = ListSpec

starred :: ListSpec
starred = ListSpec

-- | Provider configuration
data Gmail = Gmail
    {gmail_login :: String, gmail_lists :: [ListSpec], gmail_oauth2provider :: OAuth2Provider}

-- | Provider constructor
gmail :: Gmail -> Provider
gmail Gmail{gmail_login, gmail_oauth2provider = OAuth2Provider{getAccessToken}} = Provider{getLists}
  where

    tls = TLSSettingsSimple False False False
    connectionParams = ConnectionParams
        { connectionHostname = "imap.gmail.com"
        , connectionPort = 993
        , connectionUseSecure = Just tls
        , connectionUseSocks = Nothing
        }
    imapSettings = Nothing

    getLists = do
        let serviceName = "Gmail"
            tokenRequest = TokenRequest
                { clientRegisterPage = "https://console.developers.google.com/apis/credentials"
                , scopes = gmailScopes
                , serviceName
                , userId = gmail_login
                }
        accessToken <- lift $ ByteString.pack <$> getAccessToken tokenRequest
        conn <- lift $ connectServer connectionParams imapSettings
        let authRequest = mconcat
                [ "user=", ByteString.pack gmail_login, "\1"
                , "auth=Bearer ", accessToken, "\1\1" ]
            authRequestEncoded = Base64.encode authRequest
        authResult <- imap . sendCommand conn $ "AUTHENTICATE XOAUTH2 " <> authRequestEncoded
        assertE (isExpectedAuthResult authResult) $ "authentication problem: " <> show authResult
        folders <- imap $ list conn "*"
        logoutResult <- imap $ logout conn
        assertE (logoutResult == [Bye]) $ "logout failed: " <> show logoutResult
        pure [ItemList{name = serviceName, items = fmap (Item . show) folders}]
        -- fail $ "not implemented Gmail.getLists" <> show folders

gmailScopes :: [OAuth2Scope]
gmailScopes = ["https://mail.google.com/"]

imap :: ListT IO CommandResult -> Script [UntaggedResult]
imap = ExceptT . fmap (fmapL Text.unpack) . simpleFormat

assertE :: Monad m => Bool -> String -> ExceptT String m ()
assertE True _ = pure ()
assertE False e = throwE $ "Gmail: " <> e

isExpectedAuthResult :: [UntaggedResult] -> Bool
isExpectedAuthResult [OKResult{}, Capabilities{}] = True
isExpectedAuthResult _ = False
