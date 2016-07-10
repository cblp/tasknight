#!/usr/bin/env stack
-- stack runhaskell --package=optparse-applicative
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Arrow       (second)
import Control.Monad       (when)
import Data.Foldable       (traverse_)
import Data.List           (stripPrefix)
import Data.Maybe          (mapMaybe)
import Data.Monoid         ((<>))
import Options.Applicative (ParserInfo, execParser, fullDesc, header, help, helper, info, long,
                            short, switch)
import System.Environment  (setEnv)
import System.Info         (os)
import System.Process      (callProcess, readProcess, showCommandForUser)

dockerfileDir :: FilePath
dockerfileDir = "docker"

dockerImage :: String
dockerImage = "cblp/tasknight-build"

data Options = Options{options_docker :: Bool, options_setup :: Bool, options_test :: Bool}
    deriving Show

program :: ParserInfo Options
program = info (helper <*> options) $ fullDesc <> header "build helper"
  where
    options = Options
        <$> switch (short 'd' <> long "docker" <> help "use Docker")
        <*> switch (short 's' <> long "setup" <> help "run `stack setup`")
        <*> switch (short 't' <> long "test" <> help "run tests")

main :: IO ()
main = do
    Options{options_docker, options_setup, options_test} <- execParser program
    let stack = stackCommand (if options_docker then Just dockerImage else Nothing)

    -- setup environment
    when options_docker $ do
        case os of
            "darwin"  -> applyDockerMachineEnv
            "linux"   -> pure ()  -- Docker must be installed
            _         -> error $ "unsupported OS " <> os
        run $ dockerBuild dockerImage dockerfileDir

    -- install GHC
    when options_setup .
        run $ stack "setup"

    -- build project
    run $ stack "build"

    -- run tests
    when options_test .
        run $ stack "test"

stackCommand :: Maybe FilePath -> String -> Command
stackCommand mDockerImage subcommand = let
    dockerOpts = case mDockerImage of
        Nothing         -> []
        Just imageName  -> ["--docker", "--docker-image=" <> imageName]
    in
    Command "stack" $ dockerOpts <> [subcommand]

logSubprocess :: Command -> IO ()
logSubprocess = putStrLn . ("+ " <>) . showProc
  where
    showProc (Command prog args) = showCommandForUser prog args

run :: Command -> IO ()
run cmd@(Command prog args) = do
    logSubprocess cmd
    callProcess prog args

applyDockerMachineEnv :: IO ()
applyDockerMachineEnv = getDockerEnv >>= applyEnv
  where
    getDockerEnv = do
        let prog = "docker-machine"
            args = ["env", "--shell=cmd", "default"]
        logSubprocess $ Command prog args
        parseEnv <$> readProcess prog args ""
    parseEnv = mapMaybe (fmap (second tail . break ('=' ==)) . stripPrefix "SET ") . lines
    applyEnv = traverse_ $ uncurry setEnv

dockerBuild :: String -> FilePath -> Command
dockerBuild imageName imageDir = Command "docker" ["build", "--tag=" <> imageName, imageDir]

-- | like RawCommand
data Command = Command FilePath [String]
