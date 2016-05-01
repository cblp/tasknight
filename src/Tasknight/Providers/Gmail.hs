{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Providers.Gmail
    (Gmail(..), ListSpec, foldersList, gmail, inboxUnread, starred) where

import           Control.Error                         (ExceptT(..), Script, fmapL, throwE)
import           Control.Monad.Trans.Class             (lift)
import qualified Data.ByteString.Base64                as Base64
import qualified Data.ByteString.Char8                 as ByteString
import           Data.Monoid                           ((<>))
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Traversable                      (for)
import           ListT                                 (ListT)
import           Network.Connection
import           Network.IMAP
import           Network.IMAP.Types
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Rfc2822 (Field(Subject), GenericMessage(Message),
                                                        message)

import Tasknight.OAuth2   (OAuth2Provider(..), OAuth2Scope, TokenRequest(..))
import Tasknight.Provider (Item(..), ItemList(..), Provider(..))

-- | Provider configuration
data Gmail = Gmail
    {gmail_login :: String, gmail_lists :: [ListSpec], gmail_oauth2provider :: OAuth2Provider}

data Folder = Folder
    {folder_name :: Text, folder_specialName :: Maybe Text, folder_flags :: [NameAttribute]}

-- | List specificator
newtype ListSpec = ListSpec (IMAPConnection -> [Folder] -> Script [ItemList])

-- | Special list containing all folders, for debugging needs
foldersList :: ListSpec
foldersList = ListSpec $ \_conn folders -> do
    let folderItems =
            [ Item $ folder_name
                  <> maybe "" (" | " <>) folder_specialName
                  <> " | " <> Text.pack (show folder_flags)
            | Folder{folder_name, folder_specialName, folder_flags} <- folders
            ]
    pure [ItemList{name = "Folders", items = folderItems}]

-- | Unread messages in inbox
inboxUnread :: ListSpec
inboxUnread = ListSpec $ \conn _folders -> do
    let inbox = "INBOX"
    examineResult <- imap "examine from inbox" $ examine conn inbox
    searchResult <- imap "search unseen" $ search conn "UNSEEN"
    msgids <- case searchResult of
        [Search msgids] -> pure $ take 10 msgids
        []              -> pure []
        _               -> fail $ "searchResult = " <> show searchResult
    fetchedMessages <- for msgids $ \msgid ->
        imap "fetch messages" . fetchG conn $ Text.pack (show msgid) <> " (BODY.PEEK[HEADER])"
    let msgs =  [ subject
                | messageItems <- fetchedMessages
                , Fetch itemProperties <- messageItems
                , Body body <- itemProperties
                , Right msg <- pure . parse message "message body" $ ByteString.unpack body
                  -- ^ TODO(cblp, 2016-04-30) log error if left
                , Message fields _body <- pure msg
                , Subject subject <- fields
                ]
    pure  [ ItemList  { name = "Mailbox description (examine) for " <> inbox
                      , items = [Item . Text.pack $ show item | item <- examineResult]
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
        authResult <- imap "authenticate" . sendCommand conn $ "AUTHENTICATE XOAUTH2 " <> authRequestEncoded
        assertE (isExpectedAuthResult authResult) $ "authentication problem: " <> show authResult
        listResult <- imap "list *" (list conn "*")

        let folders = [ Folder  { folder_name = inboxName
                                , folder_specialName = specialName flags
                                , folder_flags = flags
                                }
                      | ListR{inboxName, flags} <- listResult ]
        lists <- fmap mconcat . for gmail_lists $ \(ListSpec getList) ->
            getList conn folders

        logoutResult <- imap "logout" $ logout conn
        assertE (logoutResult == [Bye]) $ "logout failed: " <> show logoutResult

        pure lists

gmailScopes :: [OAuth2Scope]
gmailScopes = ["https://mail.google.com/"]

imap :: Text -> ListT IO CommandResult -> Script [UntaggedResult]
imap commandDescription = ExceptT . fmap (fmapL translateError) . simpleFormat
  where
    translateError e =
        Text.unpack $ "When executing IMAP command \"" <> commandDescription <> "\", got error: " <> e

assertE :: Monad m => Bool -> String -> ExceptT String m ()
assertE True _ = pure ()
assertE False e = throwE $ "Gmail: " <> e

isExpectedAuthResult :: [UntaggedResult] -> Bool
isExpectedAuthResult [OKResult{}, Capabilities{}] = True
isExpectedAuthResult _ = False

specialName :: [NameAttribute] -> Maybe Text
specialName []                                  = Nothing
specialName (OtherNameAttr "HasChildren"  : as) = specialName as
specialName (OtherNameAttr n              : _ ) = Just n
specialName (_                            : as) = specialName as
