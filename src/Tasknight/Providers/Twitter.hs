{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Providers.Twitter (Twitter(..), twitter) where

import Tasknight.Provider (Provider(..))

-- | Provider configuration
data Twitter = Twitter

-- | Provider constructor
twitter :: Twitter -> Provider
twitter Twitter =
    let getLists = error "Twitter.getLists"
    in Provider{getLists}
