module Main (main) where

import Tasknight.Dashboard (mainWith)
import Tasknight.Dashboard.Config (Config(..))

main :: IO ()
main = mainWith Config
