{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Util.IMAP
    ( Connection, Credentials(..), Folder(..), ImapM, NameAttribute, SearchSpec(..)
    , UntaggedResult(..)
    , authenticateOAuth2, examine, fetchHeaders, list, runImap
    , search, searchAll, searchUnseen, select
    ) where

import           Control.Error                         (ExceptT(..), Script, fmapL)
import           Control.Monad.Except                  (MonadError, throwError)
import           Control.Monad.Reader                  (ReaderT, ask, lift, runReaderT)
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Base64                as Base64
import qualified Data.ByteString.Char8                 as BSC
import           Data.Function                         ((&))
import           Data.Monoid                           ((<>))
import           Data.Text                             (Text)
import qualified Data.Text.Extra                       as Text
import           ListT                                 (ListT)
import           Network.Connection                    (ConnectionParams(..))
import           Network.IMAP.Types                    (CommandResult(..), IMAPConnection,
                                                        NameAttribute(..), UntaggedResult(..))
import           Text.Parsec                           (parse)
import qualified Text.ParserCombinators.Parsec.Rfc2822 as Rfc2822

import qualified Network.IMAP as Impl

type Connection = IMAPConnection

type MessageId = Int

type ImapM a = ReaderT Connection Script a

data Credentials = Credentials{user :: ByteString, accessToken :: ByteString}

data Folder = Folder
    {folder_name :: Text, folder_specialName :: Maybe Text, folder_flags :: [NameAttribute]}

data SearchSpec = SearchAll | SearchUnseen
instance Show SearchSpec where
    show SearchAll    = "ALL"
    show SearchUnseen = "UNSEEN"

-- | TODO(cblp, 2016-05-03) Specialize
type FolderAttribute = UntaggedResult

imapAction :: Text -> (Connection -> ListT IO CommandResult) -> ImapM [UntaggedResult]
imapAction commandDescription imapCommand = do
    conn <- ask
    lift . ExceptT . fmap (fmapL translateError) . Impl.simpleFormat $ imapCommand conn
  where
    translateError e = Text.unpack $
        "When executing IMAP command \"" <> commandDescription <> "\", got error: " <> e

-- | Open a folder in read-only mode.
examine :: Text -> ImapM [FolderAttribute]
examine folder = imapAction ("examine " <> folder) $ \conn -> Impl.examine conn folder

-- | Open a folder in read/write mode.
select :: Text -> ImapM [FolderAttribute]
select folder = imapAction ("select " <> folder) $ \conn -> Impl.select conn folder

-- | Search messages.
search :: SearchSpec -> ImapM [MessageId]
search searchSpec = do
    searchResult <- imapAction ("search " <> criteria) $ \conn -> Impl.search conn criteria
    case searchResult of
        []              -> pure []
        [Search msgids] -> pure msgids
        _               -> fail $ show searchResult
  where
    criteria = Text.show searchSpec

-- | List all messages.
searchAll :: ImapM [MessageId]
searchAll = search SearchAll

-- | List unseen (unread) messages.
searchUnseen :: ImapM [MessageId]
searchUnseen = search SearchUnseen

fetchHeaders :: Int -> ImapM [Rfc2822.Field]
fetchHeaders msgid = do
    fetchResult <- imapAction "fetch messages" $
        \conn -> Impl.fetchG conn $ Text.pack (show msgid) <> " (BODY.PEEK[HEADER])"
    properties <- case fetchResult of
        [Fetch properties]  -> pure properties
        _                   -> fail $ "IMAP fetchResult: " <> show fetchResult
    messageRaw <- case properties of
        [MessageId _, Body message] -> pure message
        _                           -> fail $ "IMAP fetched properties: " <> show properties
    let message = messageRaw & BS.map (min 0x7F) & BSC.unpack
    Rfc2822.Message fields _body <-
        case parse Rfc2822.message "message" message of
            Left err  -> fail $ "IMAP fetch: parse message:\n" <> show message <> "\n" <> show err
            Right msg -> pure msg
    pure fields

authenticateOAuth2 :: Credentials -> ImapM ()
authenticateOAuth2 Credentials{user, accessToken} = do
    let authRequest = mconcat ["user=", user, "\1", "auth=Bearer ", accessToken, "\1\1"]
        authRequestEncoded = Base64.encode authRequest
    authResult <- imapAction "authenticate" $ \conn ->
        Impl.sendCommand conn $ "AUTHENTICATE XOAUTH2 " <> authRequestEncoded
    assert (isExpectedAuthResult authResult) $ "authentication problem: " <> show authResult

-- | List folders (mailboxes).
list :: ImapM [Folder]
list = do
    listResult <- imapAction "list *" $ \conn -> Impl.list conn "*"
    pure  [ Folder  { folder_name = inboxName
                    , folder_specialName = specialName flags
                    , folder_flags = flags
                    }
          | ListR{inboxName, flags} <- listResult ]

logout :: ImapM ()
logout = do
    res <- imapAction "logout" $ \conn -> Impl.logout conn
    assert (res == [Bye]) $ "logout failed: " <> show res

-- | Connect to IMAP server, do some work and disconnect.
runImap :: ConnectionParams -> Credentials -> ImapM a -> Script a
runImap connectionParams credentials body = do
    conn <- lift $ Impl.connectServer connectionParams imapSettings
    runReaderT session conn
  where
    imapSettings = Nothing
    session = do
        authenticateOAuth2 credentials
        r <- body
        logout
        pure r

assert :: MonadError String m => Bool -> String -> m ()
assert True _ = pure ()
assert False e = throwError $ "IMAP: " <> e

isExpectedAuthResult :: [UntaggedResult] -> Bool
isExpectedAuthResult [OKResult{}, Capabilities{}] = True
isExpectedAuthResult _ = False

specialName :: [NameAttribute] -> Maybe Text
specialName []                                  = Nothing
specialName (OtherNameAttr "HasChildren"  : as) = specialName as
specialName (OtherNameAttr n              : _ ) = Just n
specialName (_                            : as) = specialName as
