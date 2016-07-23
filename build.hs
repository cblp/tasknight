#!/usr/bin/env stack
-- stack runhaskell --package=optparse-applicative
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception   (bracket)
import Control.Monad       (when)
import Data.Monoid         ((<>))
import Options.Applicative (ParserInfo, execParser, fullDesc, header, help, helper, info, long,
                            short, switch)
import System.Directory    (getCurrentDirectory, setCurrentDirectory)
import System.Process      (callProcess, showCommandForUser)

data Options = Options
    { o_setupStack        :: Bool
    , o_test              :: Bool
    , o_setupTestDatabase :: Bool
    , o_yesodDevel        :: Bool
    , o_runFrontend       :: Bool
    }
    deriving Show

program :: ParserInfo Options
program = info (helper <*> options) $ fullDesc <> header "build helper"
  where
    options = Options
        <$> switch (short 's' <> long "setup"         <> help "run `stack setup` before build")
        <*> switch (short 't' <> long "test"          <> help "run tests after build")
        <*> switch (short 'b' <> long "setup-db"      <> help "setup test database before test")
        <*> switch (short 'y' <> long "yesod-devel"   <> help "run `yesod devel` after all")
        <*> switch (short 'f' <> long "run-frontend"  <> help "run frontend after all")

-- | isomorphic to System.Process.CmdSpec.RawCommand
data Command = Command FilePath [String]

data StackCommand = SBuild
                  | SExec{additionalPackages :: [String], subCommand :: Command}
                  | SSetup
                  | STest

stack :: StackCommand -> Command
stack cmd = Command "stack" $ case cmd of
    SBuild ->
        ["build"]
    SExec{additionalPackages, subCommand = Command prog args} ->
        "exec" : ["--package=" <> p | p <- additionalPackages] <> ("--" : prog : args)
    SSetup ->
        ["setup"]
    STest ->
        ["test"]

logSubprocess :: Command -> IO ()
logSubprocess = putStrLn . ("+ " <>) . showProc
  where
    showProc (Command prog args) = showCommandForUser prog args

run :: Command -> IO ()
run cmd@(Command prog args) = do
    logSubprocess cmd
    callProcess prog args

main :: IO ()
main = do
    Options{..} <- execParser program

    -- install GHC
    when o_setupStack . run $ stack SSetup

    -- build project
    run $ stack SBuild

    -- setup database
    when o_setupTestDatabase . run $ Command "./db-devel-init.sh" []

    -- run tests
    when o_test . run $ stack STest

    when o_yesodDevel .
        withCurrentDirectory "frontend" .
            run . stack . SExec ["yesod-bin", "cabal-install"] $ Command "yesod" ["devel"]

    when o_runFrontend .
        withCurrentDirectory "frontend" .
            run . stack . SExec [] $ Command "tasknight-frontend" []

-- backported from directory-1.2.3, not in LTS yet (as of lts-6.8) (in Nightly already)
-- TODO(cblp, 2016-07-22) remove
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
    bracket getCurrentDirectory setCurrentDirectory $ \ _ -> setCurrentDirectory dir >> action
