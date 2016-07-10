#!/usr/bin/env stack
-- stack runhaskell --package=optparse-applicative
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Arrow       (second)
import Control.Monad       (void, when)
import Data.Foldable       (traverse_)
import Data.List           (stripPrefix)
import Data.Maybe          (mapMaybe)
import Data.Monoid         ((<>))
import Options.Applicative (ParserInfo, execParser, fullDesc, header, help, helper, info, long,
                            short, switch)
import System.Environment  (setEnv)
import System.Info         (os)
import System.Process      (CmdSpec(..), CreateProcess(cmdspec), proc, readCreateProcess,
                            showCommandForUser)

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
            "darwin"  -> getDockerEnv >>= applyEnv
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

stackCommand :: Maybe FilePath -> String -> CreateProcess
stackCommand mDockerImage subcommand = let
    dockerOpts = case mDockerImage of
        Nothing         -> []
        Just imageName  -> ["--docker", "--docker-image=" <> imageName]
    in
    proc "stack" $ dockerOpts <> [subcommand]

logSubprocess :: CreateProcess -> IO ()
logSubprocess = putStrLn . ("+ " <>) . showProc . cmdspec
  where
    showProc (ShellCommand cmd)     = cmd
    showProc (RawCommand prog args) = showCommandForUser prog args

run :: CreateProcess -> IO ()
run cp = do
    logSubprocess cp
    void $ readCreateProcess cp ""

getDockerEnv :: IO [(String, String)]
getDockerEnv = do
    let cmd = proc "docker-machine" ["env", "--shell=cmd", "default"]
    logSubprocess cmd
    parseEnv <$> readCreateProcess cmd ""

applyEnv :: [(String, String)] -> IO ()
applyEnv = traverse_ $ uncurry setEnv

parseEnv :: String -> [(String, String)]
parseEnv = mapMaybe parseEnvLine1 . lines
  where
    parseEnvLine1 (stripPrefix "SET " -> Just line2)  = parseEnvLine2 line2
    parseEnvLine1 _                                   = Nothing
    parseEnvLine2 = Just . second tail . break ('=' ==)

dockerBuild :: String -> FilePath -> CreateProcess
dockerBuild imageName imageDir = proc "docker" ["build", "--tag=" <> imageName, imageDir]
