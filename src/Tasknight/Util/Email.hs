module Tasknight.Util.Email (subject) where

import Data.Foldable                         (fold)
import Text.ParserCombinators.Parsec.Rfc2822 (Field(Subject))

subject :: [Field] -> String
subject fields = fold [text | Subject text <- fields]
