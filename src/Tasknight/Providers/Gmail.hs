module Tasknight.Providers.Gmail (Gmail(..)) where

import Tasknight.Provider (IsProvider)

data Gmail = Gmail { login :: String }

instance IsProvider Gmail
