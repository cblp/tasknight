{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Dashboard (mainWith) where

import Data.Foldable
import Data.Traversable

import Tasknight.Dashboard.Config (Config(..))
import Tasknight.Provider (Provider(..))

mainWith :: Config -> IO ()
mainWith Config{providers} = do
    lists <- mconcat <$> for providers getLists
    for_ lists print
