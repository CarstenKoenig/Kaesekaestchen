{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Lib
    ( startApp
    , app
    ) where

import           GHC.Generics (Generic)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Elm (ElmType(..), Spec(Spec))
import qualified Elm
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Elm (Proxy(Proxy))
import qualified Servant.Elm as Elm

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]



newtype TurnToken = TurnToken String
  deriving (Show, Eq, Generic)

instance ElmType TurnToken
instance ToJSON TurnToken
instance FromJSON TurnToken


newToken :: IO TurnToken
newToken = TurnToken . UUID.toString <$> UUID.nextRandom


