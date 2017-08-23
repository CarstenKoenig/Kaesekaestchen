{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.String (fromString)
import Test.Hspec
import Test.Hspec.Wai


main :: IO ()
main = hspec spec 

spec :: Spec
spec = with (return app) $
    describe "GET /api/games" $ do
        it "responds with 200" $
            get "/api/games" `shouldRespondWith` 200
        it "responds with empty [String]" $
            get "/api/games" `shouldRespondWith` jsonResponse ([] :: [String])


jsonResponse :: ToJSON a => a -> ResponseMatcher
jsonResponse = fromString . unpack . encode
