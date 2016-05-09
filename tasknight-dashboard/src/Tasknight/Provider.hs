module Tasknight.Provider (Item(..), ItemList(..), Provider(..)) where

import Control.Error (Script)
import Data.Text     (Text)
import Network.URI   (URI)

data Item = Item{text :: Text, uri :: URI}

data ItemList = ItemList { name :: Text, items :: [Item] }

data Provider = Provider { getLists :: Script [ItemList] }
