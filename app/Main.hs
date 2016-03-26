{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Tasknight.Dashboard (mainWith)
import Tasknight.Dashboard.Config (Config(..), emptyConfig)
import Tasknight.Providers.Feedly (Feedly(..), feedly)
import Tasknight.Providers.Gmail (Gmail(..), gmail, inboxUnread, starred)
import Tasknight.Providers.Twitter (Twitter(..), twitter)

main :: IO ()
main = mainWith config
  where
    config = emptyConfig{providers}
    providers =
        [ gmail Gmail{login = "fromagxo", lists = [inboxUnread, starred]}
        , feedly Feedly{}
        , twitter Twitter{}
        ]
