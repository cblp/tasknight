{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Providers.Gmail
    (Gmail(..), ListSpec, foldersList, gmail, inboxUnread, starred) where

import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8     as ByteString
import           Data.Foldable             (fold)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text.Extra           as Text
import           Data.Traversable          (for)
import           Network.Connection        (ConnectionParams(..), TLSSettings(..))
import           Network.URI               (URI(..), URIAuth(..), nullURI)

import           Tasknight.OAuth2     (OAuth2Provider(..), OAuth2Scope, TokenRequest(..))
import           Tasknight.Provider   (Item(..), ItemList(..), Provider(..))
import           Tasknight.Util.Email (subject)
import           Tasknight.Util.IMAP  (Flag(Seen), Folder(..), ImapM, SearchQuery(ALLs, UNFLAG))
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
            [ Item{text, uri=gmailUri{uriFragment="#inbox"}}
            | Folder{folder_name, folder_attrs} <- folders
            , let text =  folder_name
                          -- <> maybe "" (" | " <>) folder_specialName
                          <> " | " <> Text.pack (show folder_attrs)
            ]
    pure [ItemList{name = "Folders", items = folderItems}]

-- | Generic messages spec
messagesListSpec :: [Text] -> String -> SearchQuery -> ImapM [ItemList]
messagesListSpec boxes uriFragment searchSpec =
    foldFor boxes $ \box -> do
        _boxAttributes <- IMAP.examine box
        msgids <- IMAP.search searchSpec
        subjects <- for msgids $ \msgid ->
            subject <$> IMAP.fetchHeaders msgid
        pure  [ ItemList  { name = Text.unwords [Text.show searchSpec, "messages in", box]
                          , items = [Item{text=subj, uri=gmailUri{uriFragment}} | subj <- subjects]
                          }
              ]

-- | Unread messages in inbox
inboxUnread :: ListSpec
inboxUnread = ListSpec $ \_folders -> messagesListSpec ["INBOX"] "#inbox" (UNFLAG Seen)

-- | Starred messages
starred :: ListSpec
starred = ListSpec $ \folders -> let
    starredBoxes =  [ folder_name
                    | Folder{folder_name, folder_specialName} <- folders
                    , folder_specialName == Just "Flagged" ]
    in messagesListSpec starredBoxes "#starred" ALLs

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

gmailUri :: URI
gmailUri = nullURI
    { uriScheme = "https:"
    , uriAuthority = Just URIAuth{uriUserInfo = "", uriRegName = "mail.google.com", uriPort = ""}
    , uriPath = "/mail/u/0"
    -- , uriFragment = "#inbox"
    }
