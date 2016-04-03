{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Dashboard (mainWith) where

import Control.Error (runScript)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Traversable (for)

import Tasknight.Dashboard.Config (Config(..))
import Tasknight.Provider (Provider(..))

mainWith :: Config -> IO ()
mainWith Config{providers} = runScript $ do
    lists <- mconcat <$> for providers getLists
    for_ lists $ liftIO . print
