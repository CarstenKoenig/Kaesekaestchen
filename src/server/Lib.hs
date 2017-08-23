{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Lib
    ( startApp
    , app
    ) where

import           GHC.Generics (Generic)
import           Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Elm (ElmType(..))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Elm (Proxy(Proxy))

import           Game


type API =
  "api" :> "games" :> Get '[JSON] [String]
  :<|> "api" :> "game" :> Capture "gameId" String :> Get '[JSON] (Maybe GameState)


startApp :: IO ()
startApp = run 8080 app


app :: Application
app = serve api $ enter (stateToHandler start) server
  where
    api :: Proxy API
    api = Proxy
    start :: AppState
    start = ()


server :: ServerT API StateHandler
server = getGameList :<|> getGame


getGameList :: StateHandler [String]
getGameList = return []


getGame :: String -> StateHandler (Maybe GameState)
getGame gameId = undefined


type StateHandler = State AppState


type AppState = ()


stateToHandler :: AppState -> State AppState :~> Handler
stateToHandler startWith = NT stateToHandler'
  where
    stateToHandler' comp = return (State.evalState comp startWith)


newtype TurnToken = TurnToken String
  deriving (Show, Eq, Generic)

instance ElmType TurnToken
instance ToJSON TurnToken
instance FromJSON TurnToken


newToken :: IO TurnToken
newToken = TurnToken . UUID.toString <$> UUID.nextRandom


