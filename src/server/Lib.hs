{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Lib
    ( GameId (..)
    , GameResponse (..)
    , API
    , startApp
    , app
    ) where

import           GHC.Generics (Generic)
import           Control.Concurrent.MVar.Lifted (MVar)
import qualified Control.Concurrent.MVar.Lifted as MVar 
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Aeson (ToJSON(..), FromJSON)
import qualified Data.Aeson as Aeson
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isNothing)
import           Data.Text as T
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Elm (ElmType(..))
import qualified Elm
import           Network.Socket (SockAddr)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Servant.Options (provideOptions)
import           Servant
import           Servant.Elm (Proxy(Proxy))
import           Game


type API =
  "api" :> "games" :> Get '[JSON] [GameId]
  :<|> "api" :> RemoteHost :> "game" :> Capture "gameId" GameId :> Get '[JSON] (Maybe GameResponse)
  :<|> "api" :> RemoteHost :> "game" :> "new" :> Capture "dim" Int :> Post '[JSON] GameId
  :<|> "api" :> RemoteHost :> "game" :> Capture "gameId" GameId :> "join" :> Post '[JSON] (Maybe GameResponse)
  :<|> "api" :> RemoteHost :> "game" :> Capture "gameId" GameId :> "move" :> ReqBody '[JSON] SegCoord :> Post '[JSON] (Maybe GameResponse)


startApp :: IO () 
startApp = do
  storage <- MVar.newMVar Map.empty
  run 8080
    $ logStdoutDev
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
  :<|> joinGame
  :<|> applyMove


getGameList :: StateHandler [GameId]
getGameList =
  State.gets (fmap GameId . Map.keys . Map.filter canBeJoined)
  where
    canBeJoined :: Game -> Bool
    canBeJoined = isNothing . redPlayer


getGame :: SockAddr -> GameId -> StateHandler (Maybe GameResponse)
getGame sender (GameId uid) = do
  game <- State.gets (Map.lookup uid) 
  return $ fmap (generateResponse sender) game


startGame :: SockAddr -> Int -> StateHandler GameId
startGame sender dim = do
  uid <- liftIO UUID.nextRandom
  State.modify (Map.insert uid (Game dim [] sender Nothing))
  return $ GameId uid


joinGame :: SockAddr -> GameId -> StateHandler (Maybe GameResponse)
joinGame sender (GameId uid) = do
  gameFound <- State.gets (Map.lookup uid)
  case gameFound of
    Nothing -> return Nothing
    Just game
      | bluePlayer game == sender ->
        return . Just $ generateResponse sender game
      | isNothing (redPlayer game) || redPlayer game == Just sender -> do
          let game' = game { redPlayer = Just sender }
          State.modify (Map.insert uid game')
          return . Just $ generateResponse sender game'
      | otherwise ->
        throwError (err400 { errBody = "you cannot join that game sorry" } :: ServantErr)


applyMove :: SockAddr -> GameId -> SegCoord -> StateHandler (Maybe GameResponse)
applyMove sender (GameId uid) atCoord = do
  foundGame <- State.gets (Map.lookup uid)
  case foundGame of
    Nothing -> return Nothing
    Just game -> do
      let resp = generateResponse sender game
      if yourMove resp
        then do
          let moves' = gameMoves game ++ [atCoord]
          State.modify (Map.insert uid (game { gameMoves = moves' }))
          let state' = calculateGameState (gameDimension game) moves'
              turn' = isPlayersTurn sender game state'
          return . Just $ GameResponse state' turn'
        else
          throwError (err400 { errBody = "not your turn sorry" } :: ServantErr)


type StateHandler = StateT AppState Handler


type AppState = Map UUID Game

data Game = Game
  { gameDimension :: Int
  , gameMoves :: [SegCoord]
  , bluePlayer :: SockAddr
  , redPlayer :: Maybe SockAddr
  }


data GameResponse = GameResponse
  { gameState :: GameState
  , yourMove  :: Bool
  } deriving (Eq, Show, Generic)

instance ElmType GameResponse
instance ToJSON GameResponse


generateResponse :: SockAddr -> Game -> GameResponse
generateResponse sender game =
  let state = calculateGameState (gameDimension game) (gameMoves game)
      turn = isPlayersTurn sender game state
  in GameResponse state turn


isPlayersTurn :: SockAddr -> Game -> GameState -> Bool
isPlayersTurn sender game state =
  case playersTurn state of
    Blue -> bluePlayer game == sender
    Red  -> redPlayer game  == Just sender


stateToHandler :: MVar AppState -> StateT AppState Handler :~> Handler
stateToHandler storage = NT stateToHandler'
  where
    stateToHandler' :: StateT AppState Handler res -> Handler res
    stateToHandler' comp = MVar.modifyMVar
        storage
        (\ state -> do
            (res, state') <- State.runStateT comp state
            return (state', res)
        )


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
