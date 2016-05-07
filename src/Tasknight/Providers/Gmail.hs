{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Providers.Gmail
    (Gmail(..), ListSpec, foldersList, gmail, inboxUnread, starred) where

import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8     as ByteString
import           Data.Foldable             (fold)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as Text
import           Data.Traversable          (for)
import           Network.Connection        (ConnectionParams(..), TLSSettings(..))

import           Tasknight.OAuth2     (OAuth2Provider(..), OAuth2Scope, TokenRequest(..))
import           Tasknight.Provider   (Item(..), ItemList(..), Provider(..))
import           Tasknight.Util.Email (subject)
import           Tasknight.Util.IMAP  (Folder(..), ImapM)
import qualified Tasknight.Util.IMAP  as IMAP

-- | Provider configuration
data Gmail = Gmail
    {gmail_login :: String, gmail_lists :: [ListSpec], gmail_oauth2provider :: OAuth2Provider}

-- | List specificator
newtype ListSpec = ListSpec ([Folder] -> ImapM [ItemList])

-- | Special list containing all folders, for debugging needs
foldersList :: ListSpec
foldersList = ListSpec $ \folders -> do
    let folderItems =
            [ Item $ folder_name
                  <> maybe "" (" | " <>) folder_specialName
                  <> " | " <> Text.pack (show folder_flags)
            | Folder{folder_name, folder_specialName, folder_flags} <- folders
            ]
    pure [ItemList{name = "Folders", items = folderItems}]

-- | Unread messages in inbox
inboxUnread :: ListSpec
inboxUnread = ListSpec $ \_folders -> do
    let box = "INBOX"
    _boxAttributes <- IMAP.examine box
    msgids <- IMAP.searchUnseen
    subjects <- for msgids $ \msgid ->
        subject <$> IMAP.fetchHeaders msgid
    pure [ ItemList { name = "Unread messages in " <> box
                    , items = [Item . Text.pack $ show item | item <- subjects] } ]

-- | Starred messages
starred :: ListSpec
starred = ListSpec $ \folders -> do
    let starredBoxes =  [ folder_name
                        | Folder{folder_name, folder_specialName} <- folders
                        , folder_specialName == Just "Flagged" ]
    foldFor starredBoxes $ \box -> do
        _boxAttributes <- IMAP.examine box
        msgids <- IMAP.searchAll
        subjects <- for msgids $ \msgid ->
            subject <$> IMAP.fetchHeaders msgid
        pure [ ItemList { name = "Messages in " <> box
                        , items = [Item . Text.pack $ show item | item <- subjects] } ]

-- | Provider constructor
gmail :: Gmail -> Provider
gmail Gmail{gmail_login, gmail_lists, gmail_oauth2provider = OAuth2Provider{getAccessToken}} =
    Provider{getLists}
  where
    tls = TLSSettingsSimple
        { settingDisableCertificateValidation = False
        , settingDisableSession = False
        , settingUseServerName = False
        }
    connectionParams = ConnectionParams
        { connectionHostname = "imap.gmail.com"
        , connectionPort = 993
        , connectionUseSecure = Just tls
        , connectionUseSocks = Nothing
        }

    getLists = do
        let serviceName = "Gmail"
            tokenRequest = TokenRequest
                { clientRegisterPage = "https://console.developers.google.com/apis/credentials"
                , scopes = gmailScopes
                , serviceName
                , userId = gmail_login
                }
        accessToken <- lift $ ByteString.pack <$> getAccessToken tokenRequest
        let imapCredentials =
                IMAP.Credentials{IMAP.user = ByteString.pack gmail_login, IMAP.accessToken}
        IMAP.runImap connectionParams imapCredentials $ do
            folders <- IMAP.list
            foldFor gmail_lists $ \(ListSpec getList) ->
                getList folders

gmailScopes :: [OAuth2Scope]
gmailScopes = ["https://mail.google.com/"]

foldFor :: (Traversable t, Applicative f, Monoid b) => t a -> (a -> f b) -> f b
foldFor xs f = fold <$> traverse f xs
