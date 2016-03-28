{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Tasknight.Dashboard (mainWith)
import Tasknight.Dashboard.Config (Config(..), emptyConfig)
import Tasknight.OAuth2 (defaultOauth2provider)
import Tasknight.Providers.Feedly (Feedly(..), feedly)
import Tasknight.Providers.Gmail (Gmail(..), gmail, inboxUnread, starred)
import Tasknight.Providers.Twitter (Twitter(..), twitter)

main :: IO ()
main = mainWith config
  where
    config = emptyConfig{providers}
    providers =
        [ gmail Gmail
              { gmail_lists = [inboxUnread, starred]
              , gmail_login = "fromagxo"
              , gmail_oauth2provider = defaultOauth2provider
              }
        , feedly Feedly{}
        , twitter Twitter{}
        ]
