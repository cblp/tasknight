module Tasknight.Dashboard.Config (Config(..), ResultDest(..), emptyConfig) where

import qualified Dropbox

import Tasknight.Provider (Provider)

-- | Result destination.
data ResultDest = Stdout | Dropbox Dropbox.Config

data Config = Config { providers :: [Provider], resultDest :: ResultDest }

emptyConfig :: Config
emptyConfig = Config{providers=[], resultDest=Stdout}
