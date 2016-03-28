{-# LANGUAGE NamedFieldPuns #-}

module Tasknight.OAuth2 (OAuth2Provider(..), Token, defaultOAuth2Provider) where

import Data.ByteString (ByteString)

import Tasknight.Cache (Cache(..))

type Token = ByteString

type TokenId = FilePath

data OAuth2Requester = OAuth2Requester {requestToken :: TokenId -> IO Token}

data OAuth2Provider = OAuth2Provider {getToken :: TokenId -> IO Token}

defaultOAuth2Provider :: Cache TokenId Token -> OAuth2Provider
defaultOAuth2Provider Cache{getValue} = OAuth2Provider{getToken}
  where
    getToken tokenId = do
        mtoken <- getValue tokenId <|> requestToken
        case mtoken of
            Nothing ->
            Just token ->
        error "unimplemented defaultOAuth2Provider.getToken"
