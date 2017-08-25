{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Lib
    ( GameId (..)
    , API
    , startApp
    , app
    ) where

import           GHC.Generics (Generic)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Aeson (ToJSON(..), FromJSON)
import qualified Data.Aeson as Aeson
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text as T
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Elm (ElmType(..))
import qualified Elm
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import           Network.Wai.Middleware.Servant.Options (provideOptions)
import           Servant
import           Servant.Elm (Proxy(Proxy))
import           Game


type API =
  "api" :> "games" :> Get '[JSON] [GameId]
  :<|> "api" :> "game" :> Capture "gameId" GameId :> Get '[JSON] (Maybe GameState)
  :<|> "api" :> "game" :> "new" :> Capture "dim" Int :> Post '[JSON] GameId
  :<|> "api" :> "game" :> Capture "gameId" GameId :> "move" :> ReqBody '[JSON] SegCoord :> Post '[JSON] (Maybe GameState)


startApp :: IO () 
startApp = do
  storage <- MVar.newMVar Map.empty
  run 8080
    $ cors (const $ Just policy)
    $ provideOptions (Proxy :: Proxy API)
    $ app storage
  where
  policy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "content-type" ] }  


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


getGameList :: StateHandler [GameId]
getGameList =
  State.gets (fmap GameId . Map.keys)


getGame :: GameId -> StateHandler (Maybe GameState)
getGame (GameId uid) = do
  game <- State.gets (Map.lookup uid) 
  return $ fmap (\ g -> calculateGameState (gameDimension g) (gameMoves g)) game


startGame :: Int -> StateHandler GameId
startGame dim = do
  uid <- liftIO UUID.nextRandom
  State.modify (Map.insert uid (Game dim []))
  return $ GameId uid


applyMove :: GameId -> SegCoord -> StateHandler (Maybe GameState)
applyMove (GameId uid) atCoord = do
  foundGame <- State.gets (Map.lookup uid)
  case foundGame of
    Nothing -> return Nothing
    Just game -> do
      let moves' = gameMoves game ++ [atCoord]
      State.modify (Map.insert uid (game { gameMoves = moves' }))
      return . Just $ calculateGameState (gameDimension game) moves'


type StateHandler = StateT AppState IO


type AppState = Map UUID Game

data Game = Game
  { gameDimension :: Int
  , gameMoves :: [SegCoord]
  }


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


----------------------------------------------------------------------
-- warp UUIDs as parts in the queries/results

newtype GameId = GameId UUID

instance ElmType GameId where
  toElmType _ = Elm.ElmPrimitive Elm.EString

instance ToJSON GameId where
  toJSON (GameId uid) = toJSON $ UUID.toString uid


instance FromJSON GameId where
  parseJSON obj = do
    s <- Aeson.parseJSON obj
    case UUID.fromString s of
      Nothing -> fail $ "no valid UUID json " ++ show obj
      Just uid -> return $ GameId uid


instance FromHttpApiData GameId where      
  parseUrlPiece piece = do
    s <- parseUrlPiece piece
    case UUID.fromString s of
      Nothing -> Left . T.pack $ "no valid UUID-piece " ++ show piece
      Just uid -> return $ GameId uid
