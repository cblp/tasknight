module Data.Text.Extra (module I, show) where

import           Data.Text as I
import           Prelude   (Show, (.))
import qualified Prelude

show :: Show a => a -> Text
show = pack . Prelude.show
