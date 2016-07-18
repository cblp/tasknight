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

data Options = Options
    { options_docker            :: Bool
    , options_setupStack        :: Bool
    , options_test              :: Bool
    , options_setupTestDatabase :: Bool
    , options_yesodDevel        :: Bool
    }
    deriving Show

program :: ParserInfo Options
program = info (helper <*> options) $ fullDesc <> header "build helper"
  where
    options = Options
        <$> switch (short 'd' <> long "docker"      <> help "use Docker")
        <*> switch (short 's' <> long "setup"       <> help "run `stack setup`")
        <*> switch (short 't' <> long "test"        <> help "run tests")
        <*> switch (short 'b' <> long "setup-db"    <> help "setup test database")
        <*> switch (short 'y' <> long "yesod-devel" <> help "run `yesod devel`")

main :: IO ()
main = do
    Options
        { options_docker
        , options_setupStack
        , options_test
        , options_setupTestDatabase
        , options_yesodDevel
        } <-
            execParser program
    let dockerParams dp_startServices =
            if options_docker
                then Just DockerParams{dp_imageName=dockerImage, dp_startServices}
                else Nothing
        defaultDockerParams       = dockerParams False
        dockerParamsWithServices  = dockerParams True
        stack                     = stackCommand defaultDockerParams
        stackTestWithServices     = stackCommand dockerParamsWithServices ["test"]
        stackExecWithServices cmd = stackCommand dockerParamsWithServices $ ["exec", "--"] <> cmd

    -- setup environment
    when options_docker $ do
        case os of
            "darwin"  -> applyDockerMachineEnv
            "linux"   -> pure ()  -- Docker must be installed
            _         -> error $ "unsupported OS " <> os
        run $ dockerBuild dockerImage dockerfileDir

    -- install GHC
    when options_setupStack .
        run $ stack ["setup"]

    -- build project
    run $ stack ["build"]

    -- setup database
    when options_setupTestDatabase .
        run $ stackExecWithServices ["./db-devel-init.sh"]

    -- run tests
    when options_test $
        run stackTestWithServices

    when options_yesodDevel .
        run $ stackExecWithServices ["yesod", "devel"]

data DockerParams = DockerParams{dp_imageName :: String, dp_startServices :: Bool}

stackCommand :: Maybe DockerParams -> [String] -> Command
stackCommand mDockerParams subcommand = let
    dockerOpts = case mDockerParams of
        Just DockerParams{dp_imageName, dp_startServices} ->
            [ "--docker"
            , "--docker-image=" <> dp_imageName
            , "--docker-env=tasknight_docker_startServices="
              <> if dp_startServices then "1" else "0"
            ]
        Nothing -> []
    in
    Command "stack" $ dockerOpts <> subcommand

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
