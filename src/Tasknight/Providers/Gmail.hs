module Tasknight.Providers.Gmail (Gmail(..), ListSpec(..), gmail, inboxUnread, starred) where

import Tasknight.Provider (Provider(..))

-- | List specificator
data ListSpec = ListSpec

inboxUnread :: ListSpec
inboxUnread = ListSpec

starred :: ListSpec
starred = ListSpec

-- | Provider configuration
data Gmail = Gmail { login :: String, lists :: [ListSpec] }

-- | Provider constructor
gmail :: Gmail -> Provider
gmail _ = Provider
