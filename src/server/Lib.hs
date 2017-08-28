{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Lib
    ( GameId (..)
    , GameResponse (..)
    , API, RestfulApiRoutes
    , startApp
    , app
    ) where

import           GHC.Generics (Generic)
import           Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import           Control.Concurrent.MVar.Lifted (MVar)
import qualified Control.Concurrent.MVar.Lifted as MVar 
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Aeson (ToJSON(..), FromJSON, encode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isNothing)
import           Data.Text as T
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Elm (ElmType(..))
import qualified Elm
import           Lucid (Html)
import qualified Lucid as Html
import           Network.Socket (SockAddr)
import           Network.WebSockets (Connection, forkPingThread, sendTextData) 
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Servant.API.WebSocket (WebSocket)
import           Servant.Elm (Proxy(Proxy))
import           Servant.HTML.Lucid (HTML)
import           Game


-- | starts the server on Port 8080
-- logs to STDOUT and uses simple CORS
startApp :: IO () 
startApp = do
  storage <- MVar.newMVar Map.empty
  run 8080
    $ logStdoutDev
    $ cors (const $ Just policy)
    $ app storage
  where
  policy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "content-type" ] }  


-- | serves the servant-API defined through the `API` type as
-- a WAI application
-- needs a MVar that is used to store the application state
app :: MVar AppState -> Application
app storage =
  serve (Proxy :: Proxy API) $ server storage


----------------------------------------------------------------------
-- define the Web-API using types (servant)

type API =
  StaticFileRoutes
  :<|> RestfulApiRoutes
  :<|> WebsocketRoutes
  :<|> ProvidedPages


type StaticFileRoutes =
  "static" :> Raw


type ProvidedPages =
  CaptureAll "segments" Text :> Get '[HTML] (Html ())


type RestfulApiRoutes =  
  "api" :> "games" :> Get '[JSON] [GameId]
  :<|> "api" :> RemoteHost :> "game" :> Capture "gameId" GameId :> Get '[JSON] (Maybe GameResponse)
  :<|> "api" :> RemoteHost :> "game" :> "new" :> Capture "dim" Int :> Post '[JSON] GameId
  :<|> "api" :> RemoteHost :> "game" :> Capture "gameId" GameId :> "join" :> Post '[JSON] (Maybe GameResponse)
  :<|> "api" :> RemoteHost :> "game" :> Capture "gameId" GameId :> "move" :> ReqBody '[JSON] SegCoord :> Post '[JSON] (Maybe GameResponse)


type WebsocketRoutes =
  "api" :> RemoteHost :> "game" :> Capture "gameId" GameId :> "subscribe" :> WebSocket


----------------------------------------------------------------------
-- handlers for the various parts of the servant application

-- | Handler of this type might modify the App-State
type StateHandler = StateT AppState Handler

-- || Handler of this type might read the App-State
type ReaderHandler = ReaderT AppState Handler


server :: MVar AppState -> ServerT API Handler
server storage =
  serveDirectoryFileServer "static"
  :<|> enter (stateToHandler storage) apiHandler
  :<|> enter (readerToHandler storage) websocketRoutesHandler
  :<|> pageHandler


pageHandler :: ServerT ProvidedPages Handler
pageHandler = provideIndex
  where
    provideIndex :: [Text] -> Handler (Html ())
    provideIndex _ = liftIO indexPage


indexPage :: IO (Html ())
indexPage = do
  content <- BS.readFile "index.html"
  return $ Html.toHtmlRaw content


apiHandler :: ServerT RestfulApiRoutes StateHandler
apiHandler = 
  getGameList
  :<|> getGame
  :<|> startGame
  :<|> joinGame
  :<|> applyMove


websocketRoutesHandler :: ServerT WebsocketRoutes ReaderHandler
websocketRoutesHandler =
  subscribeGame


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
  chan <- liftIO Chan.newChan
  uid <- liftIO UUID.nextRandom
  State.modify (Map.insert uid (Game dim [] sender Nothing chan))
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
          liftIO $ Chan.writeChan (channel game) (game, state')
          return . Just $ GameResponse state' turn'
        else
          throwError (err400 { errBody = "not your turn sorry" } :: ServantErr)


subscribeGame :: SockAddr -> GameId -> Connection -> ReaderHandler ()
subscribeGame sender (GameId uid) con = do
  gameFound <- Reader.asks (Map.lookup uid)
  case gameFound of
    Nothing ->
      lift $ throwError (err500 { errBody = "game not found" })
    Just game ->
      subscribeChannel toResponse con (channel game)
  where
    toResponse :: (Game, GameState) -> GameResponse
    toResponse (game, state) =
      let yourTurn = isPlayersTurn sender game state
      in GameResponse state yourTurn


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


readerToHandler :: MVar AppState -> ReaderT AppState Handler :~> Handler
readerToHandler storage = NT readerToHandler'
  where
    readerToHandler' :: ReaderT AppState Handler res -> Handler res
    readerToHandler' comp = do
      state <- MVar.readMVar storage
      Reader.runReaderT comp state


----------------------------------------------------------------------
-- Application State

-- | the application-state is just a map from the games-Id (UUID) to a game record
type AppState = Map UUID Game

-- | State of a single Game
data Game = Game
  { gameDimension :: Int
  , gameMoves :: [SegCoord]
  , bluePlayer :: SockAddr
  , redPlayer :: Maybe SockAddr
  , channel :: Chan (Game, GameState)
  }


-- | the game-information that is transfered to the client
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


----------------------------------------------------------------------
-- we use a simple Channel approach here to notify clients about game-changes
-- via the websocket-route above


subscribeChannel :: (Show b, ToJSON b, MonadIO m) => (a -> b) -> Connection -> Chan a -> m ()
subscribeChannel f con chan = liftIO $ do
  duped <- Chan.dupChan chan
  forkPingThread con 10
  loop duped
  where
    loop c = do
      a <- Chan.readChan c
      let b = f a
      sendTextData con (encode b)
      loop c
      

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
