#!/usr/bin/env stack
-- stack runhaskell --package=optparse-applicative
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

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
import System.Process      (callProcess, readProcess)

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
    let stack = buildStackCommand options_docker

    -- setup environment
    when options_docker $
        case os of
            "darwin"  -> getDockerEnv >>= applyEnv
            "linux"   -> pure ()  -- Docker must be installed
            _         -> error $ "unsupported OS " <> os

    -- install GHC
    when options_setup .
        run $ stack "setup"

    -- build project
    run $ stack "build"

    -- run tests
    when options_test .
        run $ stack "test"

buildStackCommand :: Bool -> String -> (String, [String])
buildStackCommand useDocker subcommand = ("stack", dockerOpts <> [subcommand])
  where
    dockerOpts = if useDocker then ["--docker", "--docker-image=" <> dockerImage] else []

logSubprocess :: [String] -> IO ()
logSubprocess prog = putStrLn . unwords $ "+" : fmap show prog

run :: (FilePath, [String]) -> IO ()
run (prog, args) = do
    logSubprocess $ prog : args
    callProcess prog args

getDockerEnv :: IO [(String, String)]
getDockerEnv = do
    let dm = "docker-machine"
        args = ["env", "--shell=cmd", "default"]
    logSubprocess $ dm : args
    parseEnv <$> readProcess dm args ""

applyEnv :: [(String, String)] -> IO ()
applyEnv = traverse_ $ uncurry setEnv

parseEnv :: String -> [(String, String)]
parseEnv = mapMaybe parseEnvLine1 . lines
  where
    parseEnvLine1 (stripPrefix "SET " -> Just line2)  = parseEnvLine2 line2
    parseEnvLine1 _                                   = Nothing
    parseEnvLine2 = Just . second tail . break ('=' ==)
