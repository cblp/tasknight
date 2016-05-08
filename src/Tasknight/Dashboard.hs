{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Dashboard (mainWith) where

import           Control.Error             (runScript)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as ByteString
import           Data.Traversable          (for)
import           Data.Yaml                 (object, (.=))
import qualified Data.Yaml.Pretty          as Yaml

import Tasknight.Dashboard.Config (Config(..))
import Tasknight.Provider         (Item(..), ItemList(..), Provider(..))

mainWith :: Config -> IO ()
mainWith Config{providers} = runScript $ do
    lists <- mconcat <$> for providers getLists
    lift . ByteString.putStrLn $ showDashboard lists

showDashboard :: [ItemList] -> ByteString
showDashboard lists =
    Yaml.encodePretty Yaml.defConfig $ object
        [ name .= [object [text .= show uri] | Item{text, uri} <- items]
        | ItemList{name, items} <- lists ]
