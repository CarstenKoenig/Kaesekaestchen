{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Lib (GameId, app)

import qualified Control.Concurrent.MVar as MVar
import           Data.Aeson (ToJSON, encode)
import           Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Map as Map
import           Data.String (fromString)
import           Test.Hspec
import           Test.Hspec.Wai


main :: IO ()
main = hspec spec

spec :: Spec
spec = with setupApp $
    describe "GET /api/games" $ do
        it "responds with 200" $
            get "/api/games" `shouldRespondWith` 200
        it "responds with empty [GameId]" $
            get "/api/games" `shouldRespondWith` jsonResponse ([] :: [GameId])
  where
    setupApp = do
      storage <- MVar.newMVar Map.empty
      return $ app storage


jsonResponse :: ToJSON a => a -> ResponseMatcher
jsonResponse = fromString . unpack . encode
