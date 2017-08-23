{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Lib
    ( startApp
    , app
    ) where

import           GHC.Generics (Generic)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Aeson
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.UUID (UUID)
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
  :<|> "api" :> "game" :> "new" :> Post '[JSON] String
  :<|> "api" :> "game" :> Capture "gameId" String :> "move" :> ReqBody '[JSON] SegCoord :> Post '[JSON] (Maybe GameState)


startApp :: IO ()
startApp = do
  storage <- MVar.newMVar Map.empty
  run 8080 $ app storage


app :: MVar AppState -> Application
app storage =
  serve api $ enter (stateToHandler storage) server
  where
    api :: Proxy API
    api = Proxy


server :: ServerT API StateHandler
server =
  getGameList
  :<|> getGame
  :<|> startGame
  :<|> applyMove


getGameList :: StateHandler [String]
getGameList =
  State.gets (fmap UUID.toString . Map.keys)


getGame :: String -> StateHandler (Maybe GameState)
getGame gameId =
  State.gets (\m -> UUID.fromString gameId >>= fmap calculateGameState . flip Map.lookup m)


startGame :: StateHandler String
startGame = do
  uid <- liftIO UUID.nextRandom
  State.modify (Map.insert uid [])
  return $ UUID.toString uid


applyMove :: String -> SegCoord -> StateHandler (Maybe GameState)
applyMove gameId atCoord =
  case UUID.fromString gameId of
    Nothing -> return Nothing
    Just uid -> do
      foundMoves <- State.gets (Map.lookup uid)
      case foundMoves of
        Nothing -> return Nothing
        Just moves -> do
          let moves' = moves ++ [atCoord]
          State.modify (Map.insert uid moves')
          return . Just $ calculateGameState moves'


type StateHandler = StateT AppState IO


type AppState = Map UUID [SegCoord]


stateToHandler :: MVar AppState -> StateT AppState IO :~> Handler
stateToHandler storage = NT stateToHandler'
  where
    stateToHandler' comp = liftIO $ do
      state <- MVar.takeMVar storage
      (res, state') <- State.runStateT comp state
      MVar.putMVar storage state'
      return res


newtype TurnToken = TurnToken String
  deriving (Show, Eq, Generic)

instance ElmType TurnToken
instance ToJSON TurnToken
instance FromJSON TurnToken


newToken :: IO TurnToken
newToken = TurnToken . UUID.toString <$> UUID.nextRandom


