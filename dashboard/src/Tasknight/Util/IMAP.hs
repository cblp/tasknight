{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Tasknight.Util.IMAP
    ( Credentials(..), Flag(Seen), Folder(..), IMAPConnection, ImapM, SearchQuery(ALLs, UNFLAG)
    , authenticateOAuth2, examine, fetchHeaders, list, runImap
    , search, searchAll, searchUnseen, select
    ) where

import           Control.Error                         (ExceptT(..), fmapLT, tryIO)
import           Control.Exception                     (IOException)
import           Control.Monad.Reader                  (ReaderT, ask, lift, runReaderT)
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Char8                 as BSC
import           Data.Function                         ((&))
import           Data.Monoid                           ((<>))
import           Network.HaskellNet.IMAP               (Attribute, AuthType(OAUTH2), Flag(Seen),
                                                        SearchQuery(ALLs, UNFLAG))
import           Network.HaskellNet.IMAP.Connection    (IMAPConnection)
import           Network.HaskellNet.IMAP.Types         (MailboxName, UID)
import           Text.Parsec                           (parse)
import qualified Text.ParserCombinators.Parsec.Rfc2822 as Rfc2822

import qualified Network.HaskellNet.IMAP as Impl

type ImapM a = ReaderT IMAPConnection (ExceptT (String, IOException) IO) a

data Credentials = Credentials{user :: String, accessToken :: String}

data Folder = Folder
    { folder_name  :: MailboxName
    -- , folder_specialName :: Maybe MailboxName
    , folder_attrs :: [Attribute]
    }

imapAction :: String -> (IMAPConnection -> IO a) -> ImapM a
imapAction commandDescription imapCommand = do
    conn <- ask
    lift . fmapLT (commandDescription,) . tryIO $ imapCommand conn
    -- lift . ExceptT . fmap (fmapL translateError) $ imapCommand conn
  -- where
  --   translateError e =
  --       "When executing IMAP command \"" <> commandDescription <> "\", got error: " <> e

-- | Open a folder in read-only mode.
examine :: MailboxName -> ImapM ()
examine folder = imapAction ("examine " <> folder) $ \conn -> Impl.examine conn folder

-- | Open a folder in read/write mode.
select :: MailboxName -> ImapM ()
select folder = imapAction ("select " <> folder) $ \conn -> Impl.select conn folder

-- | Search messages.
search :: SearchQuery -> ImapM [UID]
search searchSpec =
    imapAction ("search " <> show searchSpec) $ \conn -> Impl.search conn [searchSpec]

-- | List all messages.
searchAll :: ImapM [UID]
searchAll = search ALLs

-- | List unseen (unread) messages.
searchUnseen :: ImapM [UID]
searchUnseen = search $ UNFLAG Seen

fetchHeaders :: UID -> ImapM [Rfc2822.Field]
fetchHeaders msgid = do
    rawHeader1 <- imapAction "fetch messages" $ \conn -> Impl.fetchHeader conn msgid
    let rawHeader2 =  rawHeader1
                      & BS.map (min 0x7F) -- ignore non-encoded characters
                      & BSC.unpack
    Rfc2822.Message fields _body <-
        case parse Rfc2822.message "message" rawHeader2 of
            Left err  ->
                fail $ "IMAP fetch: parse message:\n" <> show rawHeader2 <> "\n" <> show err
            Right msg ->
                pure msg
    pure fields

authenticateOAuth2 :: Credentials -> ImapM ()
authenticateOAuth2 Credentials{user, accessToken} =
    imapAction "authenticate" $ \conn -> Impl.authenticate conn OAUTH2 user accessToken

-- | List folders (mailboxes).
list :: ImapM [Folder]
list = do
    listResult <- imapAction "list" Impl.list
    pure  [ Folder  { folder_name = box
                    -- , folder_specialName = specialName attrs
                    , folder_attrs = attrs
                    }
          | (attrs, box) <- listResult ]

logout :: ImapM ()
logout = imapAction "logout" Impl.logout

-- | Connect to IMAP server, do some work and disconnect.
runImap :: String -- ^ hostname
        -> Credentials
        -> ImapM a
        -> ExceptT (String, IOException) IO a
runImap hostname credentials body = do
    conn <- lift $ Impl.connectIMAP hostname
    runReaderTF conn $ do
        authenticateOAuth2 credentials
        r <- body
        logout
        pure r
  where
    runReaderTF = flip runReaderT

-- specialName :: [Attribute] -> Maybe MailboxName
-- specialName []                                  = Nothing
-- specialName (OtherNameAttr "HasChildren"  : as) = specialName as
-- specialName (OtherNameAttr n              : _ ) = Just n
-- specialName (_                            : as) = specialName as
