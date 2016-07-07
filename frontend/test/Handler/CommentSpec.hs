module Handler.CommentSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))

import TestImport

spec :: Spec
spec = withApp $ do
    describe "valid request" $
        it "gives a 200" $ do
            get HomeR
            statusIs 200

            let message = "My message" :: Text
                body = object [ "message" .= message ]
                encoded = encode body

            request $ do
                setMethod "POST"
                setUrl CommentR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")
                -- TODO(cblp, 2016-05-22) Commented out until
                -- https://github.com/yesodweb/yesod/issues/1199
                -- addTokenFromCookie

            statusIs 200

            [Entity _id comment] <- runDB $ selectList [CommentMessage ==. message] []
            assertEqual "Should have " comment (Comment message Nothing)

    describe "invalid requests" $
        it "400s when the JSON body is invalid" $ do
            get HomeR

            let body = object [ "foo" .= ("My message" :: Value) ]
            request $ do
                setMethod "POST"
                setUrl CommentR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
                -- TODO(cblp, 2016-05-22) Commented out until
                -- https://github.com/yesodweb/yesod/issues/1199
                -- addTokenFromCookie
            statusIs 400
