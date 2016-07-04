{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Dropbox

import           Tasknight.Dashboard        (mainWith)
import           Tasknight.Dashboard.Config (Config(..), PutResultTo(..), emptyConfig)
import           Tasknight.OAuth2           (defaultOAuth2Provider)
import           Tasknight.Providers.Gmail  (Gmail(..), gmail)
import qualified Tasknight.Providers.Gmail  as Gmail
import           Tasknight.Storage.Local    (localCache, localConfig)
-- import           Tasknight.Providers.Feedly (Feedly (..), feedly)
-- import           Tasknight.Providers.Twitter (Twitter (..), twitter)

main :: IO ()
main = mainWith config
  where
    appName = "tasknight"
    config = emptyConfig{providers, putResultTo=dropbox}
    providers =
        [ gmail Gmail
              { gmail_lists = [ Gmail.inboxUnread
                              , Gmail.starred
                              -- , Gmail.foldersList
                              ]
              , gmail_login = "fromagxo"
              , gmail_oauth2provider = defaultOAuth2Provider configStorage tokenCache
              }
        -- , feedly Feedly{}
        -- , twitter Twitter{}
        ]
    configStorage = localConfig appName
    tokenCache = localCache appName
    dropbox = Dropbox Dropbox.Config
