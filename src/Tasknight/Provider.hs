module Tasknight.Provider (Item(..), ItemList(..), Provider(..)) where

import Control.Error (Script)

data Item = Item String

data ItemList = ItemList { name :: String, items :: [Item] }

data Provider = Provider { getLists :: Script [ItemList] }
