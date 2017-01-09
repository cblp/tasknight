{-# LANGUAGE TemplateHaskell #-}

module Trello.API.Types
    ( Board(..)
    ) where

import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.Text (Text)

data Board = Board
    { name :: Text
    }
    deriving (Show)

deriveFromJSON defaultOptions ''Board
