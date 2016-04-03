{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Providers.Gmail (Gmail(..), ListSpec(..), gmail, inboxUnread, starred) where

import            Data.ByteString.Base64 as Base64
import qualified  Data.ByteString.Char8 as ByteString
import            Data.Monoid ((<>))
import            Debug.Trace
import            Network.Connection
import            Network.IMAP

import Tasknight.OAuth2 (OAuth2Provider(..), OAuth2Scope, TokenRequest(..))
import Tasknight.Provider (Provider(..))

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
gmail Gmail{gmail_login, gmail_oauth2provider = OAuth2Provider{getToken}} =
    let tls = TLSSettingsSimple False False False
        connectionParams = ConnectionParams
            { connectionHostname = "imap.gmail.com"
            , connectionPort = 993
            , connectionUseSecure = Just tls
            , connectionUseSocks = Nothing
            }
        imapSettings = Nothing
        getLists = do
            let tokenRequest = TokenRequest
                    { clientRegisterPage = "https://console.developers.google.com/apis/credentials"
                    , scopes = gmailScopes
                    , serviceName = "Gmail"
                    , userId = gmail_login
                    }
            accessToken <- ByteString.pack <$> getToken tokenRequest
            conn <- connectServer connectionParams imapSettings
            let authRequest = mconcat
                    [ "user=", ByteString.pack gmail_login, "\1"
                    , "auth=Bearer ", accessToken, "\1\1" ] :: ByteString.ByteString
                authRequestEncoded = Base64.encode authRequest
            traceM "authenticate..."
            sendCommand conn ("AUTHENTICATE XOAUTH2 " <> authRequestEncoded) >>= traceShowM
            traceM "list..."
            list conn "*" >>= traceShowM
            traceM "logout..."
            logout conn >>= traceShowM
            traceM "Done."
            fail "not implemented Gmail.getLists"
    in Provider{getLists}

gmailScopes :: [OAuth2Scope]
gmailScopes = ["https://mail.google.com/"]
