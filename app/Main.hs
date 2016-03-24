module Main (main) where

import Tasknight.Dashboard (mainWith)
import Tasknight.Dashboard.Config (Config(..), emptyConfig)
import Tasknight.Provider (Provider(..))
import Tasknight.Providers.Gmail (Gmail(..))

main :: IO ()
main = mainWith emptyConfig
    { providers = [Provider $ Gmail "fromagxo"] }
