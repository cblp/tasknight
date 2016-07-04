module Tasknight.Dashboard.Config (Config(..), PutResultTo(..), emptyConfig) where

import qualified Dropbox

import Tasknight.Provider (Provider)

data PutResultTo = Stdout | Dropbox Dropbox.Config

data Config = Config { providers :: [Provider], putResultTo :: PutResultTo }

emptyConfig :: Config
emptyConfig = Config { providers = [], putResultTo = Stdout }
