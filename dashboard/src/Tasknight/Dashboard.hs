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
import           Data.Time                  (TimeZone(..), ZonedTime, getCurrentTime,
                                             utcToZonedTime)
import           Data.Traversable           (for)
import           Dropbox                    (withDropbox, writeFile)
import           Lucid                      (a_, body_, h1_, href_, html_, li_, ol_, p_, renderBS,
                                             toHtml)
import           Prelude                    hiding (writeFile)

import Tasknight.Dashboard.Config (Config(..), ResultDest(..))
import Tasknight.Provider         (Item(..), ItemList(..), Provider(..))

mainWith :: Config -> IO ()
mainWith Config{providers, resultDest} = runScript $ do
    lists <- mconcat <$> for providers getLists
    updateTime <- utcToZonedTime msk <$> lift getCurrentTime
    lift . putResult resultDest $ showDashboard updateTime lists
  where
    msk = TimeZone{timeZoneMinutes=180, timeZoneSummerOnly=False, timeZoneName="MSK"}

showDashboard :: ZonedTime -> [ItemList] -> ByteString
showDashboard updateTime lists = renderBS . html_ . body_ $ do
    for_ lists $ \ItemList{name, items} -> do
        h1_ $ toHtml name
        ol_ .
            for_ items $ \Item{text, uri} ->
                li_ . a_ [href_ $ toText uri] $ toHtml text
    p_ . toHtml $ "Updated at " <> show updateTime
  where
    toText = Text.pack . show

putResult :: ResultDest -> ByteString -> IO ()
putResult Stdout = ByteString.putStrLn
putResult (Dropbox dbConfig) = withDropbox dbConfig . writeFile "/next.html"
