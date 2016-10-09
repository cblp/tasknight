{-# LANGUAGE TemplateHaskell #-}

module Web.Trello.API.Types
  ( Board
  ) where

import Data.Aeson.TH (deriveFromJSON, defaultOptions)

data Board =
    Board
    deriving (Show)

deriveFromJSON defaultOptions ''Board
