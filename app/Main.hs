{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Tasknight.Dashboard        (mainWith)
import           Tasknight.Dashboard.Config (Config (..), emptyConfig)
import           Tasknight.OAuth2           (defaultOAuth2Provider)
-- import           Tasknight.Providers.Feedly (Feedly (..), feedly)
import           Tasknight.Providers.Gmail  (Gmail (..), gmail, inboxUnread)
-- import           Tasknight.Providers.Twitter (Twitter (..), twitter)
import           Tasknight.Storage.Local    (localCache, localConfig)

main :: IO ()
main = mainWith config
  where
    appName = "tasknight"
    config = emptyConfig{providers}
    providers =
        [ gmail Gmail
              { gmail_lists = [ inboxUnread
                              -- , starred
                              -- , foldersList
                              ]
              , gmail_login = "fromagxo"
              , gmail_oauth2provider = defaultOAuth2Provider configStorage tokenCache
              }
        -- , feedly Feedly{}
        -- , twitter Twitter{}
        ]
    configStorage = localConfig appName
    tokenCache = localCache appName
