{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Providers.Gmail
    (Gmail(..), ListSpec, foldersList, gmail, inboxUnread, starred) where

import           Control.Monad.Trans.Class             (lift)
import qualified Data.ByteString.Char8                 as ByteString
import           Data.Foldable                         (fold)
import           Data.Monoid                           ((<>))
import qualified Data.Text                             as Text
import           Data.Traversable                      (for)
import           Network.Connection                    (ConnectionParams(..), TLSSettings(..))
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Rfc2822 (Field(Subject), GenericMessage(Message),
                                                        message)

import           Tasknight.OAuth2    (OAuth2Provider(..), OAuth2Scope, TokenRequest(..))
import           Tasknight.Provider  (Item(..), ItemList(..), Provider(..))
import           Tasknight.Util.IMAP (ImapM)
import qualified Tasknight.Util.IMAP as IMAP

-- | Provider configuration
data Gmail = Gmail
    {gmail_login :: String, gmail_lists :: [ListSpec], gmail_oauth2provider :: OAuth2Provider}

-- | List specificator
newtype ListSpec = ListSpec ([IMAP.Folder] -> ImapM [ItemList])

-- | Special list containing all folders, for debugging needs
foldersList :: ListSpec
foldersList = ListSpec $ \folders -> do
    let folderItems =
            [ Item $ folder_name
                  <> maybe "" (" | " <>) folder_specialName
                  <> " | " <> Text.pack (show folder_flags)
            | IMAP.Folder{IMAP.folder_name, IMAP.folder_specialName, IMAP.folder_flags} <- folders
            ]
    pure [ItemList{name = "Folders", items = folderItems}]

-- | Unread messages in inbox
inboxUnread :: ListSpec
inboxUnread = ListSpec $ \_folders -> do
    let inbox = "INBOX"
    inboxAttributes <- IMAP.examine inbox
    msgids <- IMAP.searchUnseen
    fetchedMessages <- for msgids IMAP.fetchHeaders
    let msgs =  [ subject
                | messageItems <- fetchedMessages
                , IMAP.Fetch itemProperties <- messageItems
                , IMAP.Body body <- itemProperties
                , Right msg <- pure . parse message "message body" $ ByteString.unpack body
                  -- ^ TODO(cblp, 2016-04-30) log error if left
                , Message fields _body <- pure msg
                , Subject subject <- fields
                ]
    pure  [ ItemList  { name = "Mailbox description (examine) for " <> inbox
                      , items = [Item . Text.pack $ show item | item <- inboxAttributes]
                      }
          , ItemList  { name = "Unread messages in " <> inbox
                      , items = [Item . Text.pack $ show item | item <- msgs]
                      }
          ]

-- | Starred messages
starred :: ListSpec
starred = error "Gmail.starred"

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
