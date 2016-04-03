{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Dashboard (mainWith) where

import            Control.Error (runScript)
import            Control.Monad.Trans.Class (lift)
import            Data.ByteString (ByteString)
import qualified  Data.ByteString.Char8 as ByteString
import qualified  Data.Text as Text
import            Data.Traversable (for)
import qualified  Data.Yaml as Yaml
import            Data.Yaml ((.=), object)

import Tasknight.Dashboard.Config (Config(..))
import Tasknight.Provider (Item(..), ItemList(..), Provider(..))

mainWith :: Config -> IO ()
mainWith Config{providers} = runScript $ do
    lists <- mconcat <$> for providers getLists
    lift . ByteString.putStrLn $ showDashboard lists

showDashboard :: [ItemList] -> ByteString
showDashboard lists =
    Yaml.encode $ object
        [ Text.pack name .= [Text.pack item | Item item <- items]
        | ItemList{name, items} <- lists
        ]
