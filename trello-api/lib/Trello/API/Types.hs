{-# LANGUAGE TemplateHaskell #-}

module Trello.API.Types
    ( Board(..)
    ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Text (Text)

data Board = Board
    { name :: Text
    -- , powerUps :: [Text] -- always empty?
    }
    deriving (Show)

deriveJSON defaultOptions ''Board
