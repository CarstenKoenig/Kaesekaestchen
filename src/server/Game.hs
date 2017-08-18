{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game
  ( Player
  , Coord
  , writeElmFile
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson (encode, ToJSON(..), FromJSON(..))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Elm (ElmType(..), Spec(Spec))
import qualified Elm
import           Servant.Elm (Proxy(Proxy))
import qualified Servant.Elm as Elm


data GameState =
  GameState
  { movesMade :: [ (Player, SegCoord) ]
  , wonCells :: [ (Player, Coord) ]
  , playersTurn :: Player
  , turnToken :: Maybe TurnToken
  } deriving (Show, Eq, Generic)

instance ElmType GameState
instance ToJSON GameState


newtype TurnToken = TurnToken String
  deriving (Show, Eq, Generic)

instance ElmType TurnToken
instance ToJSON TurnToken
instance FromJSON TurnToken


data Player
  = Blue | Red
  deriving (Show, Eq, Ord, Enum, Generic)

instance ElmType Player
instance ToJSON Player
instance FromJSON Player
  

data SegCoord
  = HCoord Coord
  | VCoord Coord
  deriving (Show, Eq, Ord, Generic)

instance ElmType SegCoord
instance ToJSON SegCoord
instance FromJSON SegCoord


type Coord = (Int, Int)


toJson :: GameState -> ByteString
toJson = encode


calculateGameState :: Maybe TurnToken -> [SegCoord] -> GameState
calculateGameState token moveCoords =
  let movesMade = zip (cycle [Blue, Red]) moveCoords
      wonCells = [] -- todo calc won cells
      playersTurn = if even (length moveCoords) then Blue else Red
  in GameState movesMade wonCells playersTurn token
      

newToken :: IO TurnToken
newToken = TurnToken . UUID.toString <$> UUID.nextRandom


writeElmFile :: FilePath -> IO ()
writeElmFile = Elm.specsToDir [spec]


spec :: Spec
spec = Spec
  ["Api", "Game"]
  [ Elm.defElmImports
  , Elm.toElmTypeSource    (Proxy :: Proxy Player)
  , Elm.toElmDecoderSource (Proxy :: Proxy Player)
  , Elm.toElmTypeSource    (Proxy :: Proxy SegCoord)
  , Elm.toElmDecoderSource (Proxy :: Proxy SegCoord)
  , Elm.toElmDecoderSource (Proxy :: Proxy SegCoord)
  , Elm.toElmTypeSource    (Proxy :: Proxy GameState)
  , Elm.toElmDecoderSource (Proxy :: Proxy GameState)
  ]
