-- stack runhaskell --package=optparse-applicative
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

import Control.Monad       (when)
import Data.Monoid         ((<>))
import Options.Applicative (ParserInfo, execParser, flag, fullDesc, header,
                            help, helper, info, long, short, switch)
import System.Directory    (withCurrentDirectory)
import System.Process      (callProcess, showCommandForUser)

data Options = Options
    { o_terminal          :: Maybe Bool
    , o_setupStack        :: Bool
    , o_test              :: Bool
    , o_fileWatch         :: Bool
    , o_setupTestDatabase :: Bool
    , o_runFrontend       :: Bool
    }
    deriving (Show)

programInfo :: ParserInfo Options
programInfo =
    info (helper <*> options) $ fullDesc <> header "build and test helper"
  where
    options =
        Options
        <$> flag
            Nothing
            (Just False)
            ( long "no-terminal"
              <> help "Treat terminal as false, default: auto-detect"
            )
        <*> switch
            ( short 's' <> long "setup"
              <> help "Setup GHC and Stack before build"
            )
        <*> switch (short 't' <> long "test" <> help "Run tests after build")
        <*> switch
            ( short 'w' <> long "file-watch"
              <> help
                  "Watch for changes in local files and automatically rebuild. \
                  \(Pass `--file-watch` option to `stack`)"
            )
        <*> switch
            ( short 'b' <> long "setup-db"
              <> help "Setup test database before test" )
        <*> switch
            (short 'f' <> long "run-frontend" <> help "Run frontend after all")

-- | isomorphic to System.Process.CmdSpec.RawCommand
data Command = Command FilePath [String]

data BuildOption
    = FileWatch
    | OnlyDependencies
    | Pedantic

instance Show BuildOption where
    show FileWatch        = "--file-watch"
    show OnlyDependencies = "--only-dependencies"
    show Pedantic         = "--pedantic"

data ExecOptions = ExecOptions
    { additionalPackages :: [String]
    , subCommand         :: Command
    }

data StackCommand
    = Build [BuildOption]
    | Exec ExecOptions
    | Setup
    | Test [BuildOption]

stack :: Maybe Bool -> StackCommand -> Command
stack optTerminal cmd =
    Command "stack" . addOptTerminal $
    case cmd of
        Build opts -> "build" : map show opts
        Exec ExecOptions {additionalPackages, subCommand = Command prog args} ->
            "exec" :
            ["--package=" <> p | p <- additionalPackages] <>
            ("--" : prog : args)
        Setup -> ["setup"]
        Test opts -> "test" : map show opts
  where
    addOptTerminal = case optTerminal of
        Nothing -> id
        Just t -> \opts ->
            "--" <> (if t then "no-" else "") <> "terminal" : opts


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
    Options {..} <- execParser programInfo
    -- install GHC
    when o_setupStack . run $ stack o_terminal Setup
    -- build project
    run . stack o_terminal $ Build [OnlyDependencies]
    run . stack o_terminal $
        Build $
        Pedantic : [FileWatch | o_fileWatch && not (o_test || o_runFrontend)]
    -- setup database
    when o_setupTestDatabase . run $ Command "./db-devel-init.sh" []
    -- run tests
    when o_test . run . stack o_terminal $
        Test $ Pedantic : [FileWatch | o_fileWatch && not o_runFrontend]
    when o_runFrontend .
        withCurrentDirectory "frontend" . run . stack o_terminal $
            Exec
                ExecOptions
                    { additionalPackages = []
                    , subCommand = Command "tasknight-frontend" []
                    }
