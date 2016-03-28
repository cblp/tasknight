{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Providers.Gmail (Gmail(..), ListSpec(..), gmail, inboxUnread, starred) where

import Control.Applicative (liftA2)
import Debug.Trace
import Network.Connection
import Network.IMAP
import System.Mem.StableName (makeStableName)

import Tasknight.OAuth2 (OAuth2Provider(..))
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
            token <- getToken gmail_login
            conn <- connectServer connectionParams imapSettings
            -- traceM "login..."
            -- login conn gmail_login "?" >>= traceShowM
            traceM "authenticate..."
            authenticate conn "XOAUTH2" $ \conn' -> do
                traceShowM =<< liftA2 (==) (makeStableName conn) (makeStableName conn')
                traceM "sendCommand token..."
                sendCommand conn' token >>= traceShowM
                traceM "noop..."
                noop conn' >>= traceShowM
            traceM "noop..."
            noop conn >>= traceShowM
            traceM "list..."
            list conn "*" >>= traceShowM
            traceM "logout..."
            logout conn >>= traceShowM
            traceM "Done."
            pure []
    in Provider{getLists}
