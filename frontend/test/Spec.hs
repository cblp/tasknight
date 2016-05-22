{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Test   (Session, assertHeader, assertStatus, defaultRequest, request, runSession,
                           setPath)
import Tasknight.Frontend (app)
import Test.Tasty         (defaultMain, testGroup)
import Test.Tasty.HUnit   (testCase)

runSession' :: Session a -> IO a
runSession' session = runSession session app

main :: IO ()
main = defaultMain $ testGroup "HTTP"
    [ testCase "/ redirects to /dashboard" . runSession' $ do
          resp <- request defaultRequest
          assertStatus 303 resp
          assertHeader "Location" "/dashboard" resp
    , testCase "/dashboard unauthorized" . runSession' $ do
          resp <- request $ setPath defaultRequest "/dashboard"
          assertStatus 401 resp
    ]
