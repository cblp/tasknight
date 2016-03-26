module Tasknight.Provider (Item(..), ItemList(..), Provider(..)) where

data Item = Item
    deriving Show

data ItemList = ItemList { name :: String, items :: [Item] }
    deriving Show

data Provider = Provider { getLists :: IO [ItemList] }
