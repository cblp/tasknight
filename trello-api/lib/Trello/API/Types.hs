{-# LANGUAGE TemplateHaskell #-}

module Trello.API.Types
    ( Board
    ) where

import Data.Aeson.TH (defaultOptions, deriveFromJSON)

data Board =
    Board
    deriving (Show)

deriveFromJSON defaultOptions ''Board
