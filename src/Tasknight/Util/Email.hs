{-# LANGUAGE OverloadedStrings #-}

module Tasknight.Util.Email (subject) where

import           Data.Attoparsec.ByteString            (parseOnly)
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Char8                 as BSC
import           Data.Char                             (isSpace)
import           Data.Foldable                         (fold)
import           Data.Text                             (Text)
import           Data.Text.Encoding                    (decodeUtf8)
import qualified Data.Text.Lazy                        as TextLazy
import           Network.Email.Header.Parser           (unstructured)
import           Text.ParserCombinators.Parsec.Rfc2822 (Field(Subject))

subject :: [Field] -> Text
subject fields = fold [parsePhrase $ BSC.pack rawSubject | Subject rawSubject <- fields]

parsePhrase :: ByteString -> Text
parsePhrase raw =
    case parseOnly unstructured $ BSC.dropWhile isSpace raw of
        Left _        -> decodeUtf8 raw
        Right result  -> TextLazy.toStrict result
