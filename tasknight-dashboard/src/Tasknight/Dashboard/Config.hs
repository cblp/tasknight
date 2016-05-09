module Tasknight.Dashboard.Config (Config(..), emptyConfig) where

import Tasknight.Provider (Provider)

data Config = Config { providers :: [Provider] }

emptyConfig :: Config
emptyConfig = Config { providers = [] }
