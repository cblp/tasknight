#!/usr/bin/env stack
-- stack runhaskell --package=directory-1.2.3.0 --package=lens --package=optparse-applicative
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Arrow       (second)
import Control.Lens        (makeLenses, (&~), (.=), (?=), (^.), _Just)
import Control.Monad       (when)
import Control.Monad.State (State)
import Data.List           (stripPrefix)
import Data.Monoid         ((<>))
import Data.String         (IsString(..))
import Options.Applicative (ParserInfo, execParser, fullDesc, header, help, helper, info, long,
                            short, switch)
import System.Directory    (withCurrentDirectory)
import System.Environment  (setEnv)
import System.Info         (os)
import System.Process      (callProcess, readProcess, showCommandForUser)

dockerfileDir :: FilePath
dockerfileDir = "docker"

dockerImage :: String
dockerImage = "cblp/tasknight-build"

data Options = Options
    { o_docker            :: Bool
    , o_setupStack        :: Bool
    , o_test              :: Bool
    , o_setupTestDatabase :: Bool
    , o_yesodDevel        :: Bool
    }
    deriving Show

program :: ParserInfo Options
program = info (helper <*> options) $ fullDesc <> header "build helper"
  where
    options = Options
        <$> switch (short 'd' <> long "docker"      <> help "use Docker for build and test")
        <*> switch (short 's' <> long "setup"       <> help "run `stack setup` before build")
        <*> switch (short 't' <> long "test"        <> help "run tests after build")
        <*> switch (short 'b' <> long "setup-db"    <> help "setup test database before test")
        <*> switch (short 'y' <> long "yesod-devel" <> help "run `yesod devel` after all")

-- | like RawCommand
data Command = Command FilePath [String]
instance IsString Command where
    fromString prog = Command prog []

data StackCommand = SBuild
                  | SExec{additionalPackages :: [String], subCommand :: Command}
                  | SSetup
                  | STest

data DockerParams = DockerParams{_imageName :: String, _startServices :: Bool}
makeLenses ''DockerParams

data StackParams = StackParams{_dockerParams :: Maybe DockerParams}
makeLenses ''StackParams

defaultStackParams :: StackParams
defaultStackParams = StackParams{_dockerParams = Nothing}

stack :: State StackParams () -> StackCommand -> Command
stack paramsState cmd = let
    sp = defaultStackParams &~ paramsState
    dockerOpts = case sp ^. dockerParams of
        Just dp ->
            [ "--docker"
            , "--docker-image=" <> dp ^. imageName
            , "--docker-env=tasknight_docker_startServices="
              <> if dp ^. startServices then "1" else "0"
            ]
        Nothing -> []
    stackCommand = case cmd of
        SBuild ->
            ["build"]
        SExec{additionalPackages, subCommand = Command prog args} ->
            "exec" : ["--package=" <> p | p <- additionalPackages] <> ("--" : prog : args)
        SSetup ->
            ["setup"]
        STest ->
            ["test"]
    in
    Command "stack" $ dockerOpts <> stackCommand

logSubprocess :: Command -> IO ()
logSubprocess = putStrLn . ("+ " <>) . showProc
  where
    showProc (Command prog args) = showCommandForUser prog args

run :: Command -> IO ()
run cmd@(Command prog args) = do
    logSubprocess cmd
    callProcess prog args

applyDockerMachineEnv :: IO ()
applyDockerMachineEnv = getDockerMachineEnv >>= applyEnv
  where
    getDockerMachineEnv = do
        let prog = "docker-machine"
            args = ["env", "--shell=cmd"]
        logSubprocess $ Command prog args
        parseEnv <$> readProcess prog args ""
    parseEnv content =  [ splitBy '=' varVal
                        | line <- lines content
                        , Just varVal <- [stripPrefix "SET " line]
                        ]
    applyEnv = mapM_ $ uncurry setEnv
    splitBy c = second tail . break (c ==)

dockerBuild :: String -> FilePath -> Command
dockerBuild imageTag imageDir = Command "docker" ["build", "--tag=" <> imageTag, imageDir]

main :: IO ()
main = do
    Options{..} <- execParser program

    let withDockerIfRequested =
            when o_docker $
                dockerParams ?= DockerParams{_imageName=dockerImage, _startServices=False}
    let withServices = dockerParams ?. startServices .= True

    -- setup environment
    when o_docker $ do
        case os of
            "darwin"  -> applyDockerMachineEnv
            "linux"   -> pure ()  -- Docker must be installed
            _         -> error $ "unsupported OS " <> os
        run $ dockerBuild dockerImage dockerfileDir

    -- install GHC
    when o_setupStack .
        run $ stack withDockerIfRequested SSetup

    -- build project
    run $ stack withDockerIfRequested SBuild

    -- setup database
    when o_setupTestDatabase .
        run . stack (withDockerIfRequested >> withServices) $ SExec [] "./db-devel-init.sh"

    -- run tests
    when o_test .
        run $ stack (withDockerIfRequested >> withServices) STest

    when o_yesodDevel .
        withCurrentDirectory "frontend" .
            run . stack (withDockerIfRequested >> withServices) .
                SExec ["yesod-bin", "cabal-install"] $ Command "yesod" ["devel"]

  where
    optic1 ?. optic2 = optic1 . _Just . optic2
