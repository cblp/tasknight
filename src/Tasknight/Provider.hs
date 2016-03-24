{-# LANGUAGE ExistentialQuantification #-}

module Tasknight.Provider (IsProvider, Provider(..)) where

class IsProvider a

data Provider = forall p . IsProvider p => Provider p
