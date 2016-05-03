{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Util.IMAP
    ( Connection, Credentials(..), Folder(..), ImapM, NameAttribute, UntaggedResult(..)
    , authenticateOAuth2, examine, fetchHeaders, list, runImap, searchUnseen
    ) where

import           Control.Error                         (ExceptT(..), Script, fmapL)
import           Control.Monad.Except                  (MonadError, throwError)
import           Control.Monad.Reader                  (ReaderT, ask, lift, runReaderT)
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Base64                as Base64
import qualified Data.ByteString.Char8                 as ByteString
import           Data.Monoid                           ((<>))
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           ListT                                 (ListT)
import           Network.Connection                    (ConnectionParams(..))
import           Network.IMAP.Types                    (CommandResult(..), IMAPConnection,
                                                        NameAttribute(..), UntaggedResult(..))
import           Text.Parsec                           (parse)
import qualified Text.ParserCombinators.Parsec.Rfc2822 as Rfc2822

import qualified Network.IMAP as Impl

type Connection = IMAPConnection

type ImapM a = ReaderT Connection Script a

data Credentials = Credentials{user :: ByteString, accessToken :: ByteString}

data Folder = Folder
    {folder_name :: Text, folder_specialName :: Maybe Text, folder_flags :: [NameAttribute]}

-- | TODO(cblp, 2016-05-03) Specialize
type FolderAttribute = UntaggedResult

-- | TODO(cblp, 2016-05-03) Specialize
-- type MessageHeader = UntaggedResult

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

-- | Return message ids.
searchUnseen :: ImapM [Int]
searchUnseen = do
    searchResult <- imapAction "search unseen" $ \conn -> Impl.search conn "UNSEEN"
    case searchResult of
        []              -> pure []
        [Search msgids] -> pure msgids
        _               -> fail $ show searchResult

fetchHeaders :: Int -> ImapM [Rfc2822.Field]
fetchHeaders msgid = do
    fetchResult <- imapAction "fetch messages" $
        \conn -> Impl.fetchG conn $ Text.pack (show msgid) <> " (BODY.PEEK[HEADER])"
    properties <- case fetchResult of
        [Fetch properties]  -> pure properties
        _                   -> fail $ "IMAP fetchResult: " <> show fetchResult
    body <- case properties of
        [Body body] -> pure body
        _           -> fail $ "IMAP fetched properties: " <> show properties
    Rfc2822.Message fields _body <-
        case parse Rfc2822.message "message body" $ ByteString.unpack body of
            Left err  -> fail $ "IMAP fetch: parse message: " <> show err
            Right msg -> pure msg
    pure fields

authenticateOAuth2 :: Credentials -> ImapM ()
authenticateOAuth2 Credentials{user, accessToken} = do
    let authRequest = mconcat ["user=", user, "\1", "auth=Bearer ", accessToken, "\1\1"]
        authRequestEncoded = Base64.encode authRequest
    authResult <- imapAction "authenticate" $ \conn ->
        Impl.sendCommand conn $ "AUTHENTICATE XOAUTH2 " <> authRequestEncoded
    assert (isExpectedAuthResult authResult) $ "authentication problem: " <> show authResult

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
