{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.Dashboard (mainWith, showDashboard) where

import           Control.Error              (runScript)
import           Control.Monad.Trans.Class  (lift)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text                  as Text
import           Data.Traversable           (for)
import           Lucid                      (a_, body_, h1_, href_, html_, li_, ol_, renderBS,
                                             toHtml)

import Tasknight.Dashboard.Config (Config(..))
import Tasknight.Provider         (Item(..), ItemList(..), Provider(..))

mainWith :: Config -> IO ()
mainWith Config{providers} = runScript $ do
    lists <- mconcat <$> for providers getLists
    lift . ByteString.putStrLn $ showDashboard lists

showDashboard :: [ItemList] -> ByteString
showDashboard lists = renderBS . html_ . body_ $ mconcat
    [ do  h1_ $ toHtml name
          ol_ $ mconcat
              [ li_ . a_ [href_ $ toText uri] $ toHtml text | Item{text, uri} <- items ]
    | ItemList{name, items} <- lists
    ]
  where
    toText = Text.pack . show
