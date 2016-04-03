module Tasknight.Provider (Item(..), ItemList(..), Provider(..)) where

import Control.Error (Script)

data Item = Item String
    deriving Show

data ItemList = ItemList { name :: String, items :: [Item] }
    deriving Show

data Provider = Provider { getLists :: Script [ItemList] }
