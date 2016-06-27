{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Dashboard (mainWith, showDashboard) where

import           Control.Error              (runScript)
import           Control.Monad.Trans.Class  (lift)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Foldable              (for_)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import           Data.Time                  (UTCTime, getCurrentTime)
import           Data.Traversable           (for)
import           Lucid                      (a_, body_, h1_, href_, html_, li_, ol_, p_, renderBS,
                                             toHtml)

import Tasknight.Dashboard.Config (Config(..))
import Tasknight.Provider         (Item(..), ItemList(..), Provider(..))

mainWith :: Config -> IO ()
mainWith Config{providers} = runScript $ do
    lists <- mconcat <$> for providers getLists
    updateTime <- lift getCurrentTime
    lift . ByteString.putStrLn $ showDashboard updateTime lists

showDashboard :: UTCTime -> [ItemList] -> ByteString
showDashboard updateTime lists = renderBS . html_ . body_ $ do
    for_ lists $ \ItemList{name, items} -> do
        h1_ $ toHtml name
        ol_ .
            for_ items $ \Item{text, uri} ->
                li_ . a_ [href_ $ toText uri] $ toHtml text
    p_ . toHtml $ "Updated at " <> show updateTime
  where
    toText = Text.pack . show
