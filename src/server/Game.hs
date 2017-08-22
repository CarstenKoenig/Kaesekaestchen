{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game
  ( Player (..)
  , Coord
  , SegCoord (..)
  , GameState (..)
  , calculateGameState
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe, fromJust)
import           Elm (ElmType(..))


data GameState =
  GameState
  { movesMade :: [ (Player, SegCoord) ]
  , wonCells :: [ (Player, Coord) ]
  , playersTurn :: Player
  } deriving (Show, Eq, Generic)

instance ElmType GameState
instance ToJSON GameState


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


calculateGameState :: [SegCoord] -> GameState
calculateGameState = foldMoves


----------------------------------------------------------------------
-- manage player moves / generate state


foldMoves :: [SegCoord] -> GameState
foldMoves coords =
  let (next, moves, cells) =
        foldl' doMove (Blue, [], Map.empty) coords
  in GameState (reverse moves) (getWonCells cells) next
        
  where
    doMove (player, moves, cells) sCoord =
      let cells' = placeSegment player sCoord cells
          moves' = (player, sCoord) : moves
          wonCell = any (filled cells cells') $ getCoords sCoord
          next = nextPlayer wonCell player
      in (next, moves', cells')
    nextPlayer wonCell player =
      if wonCell
      then player
      else other player
    other Blue = Red
    other Red = Blue
    filled prev cur coord =
          won (getCell coord cur)
          && not (won $ getCell coord prev)
    getCoords (HCoord (x,y)) = [ (x,y), (x,y-1) ]
    getCoords (VCoord (x,y)) = [ (x,y), (x-1,y) ]
    getCell coord cells =
      fromMaybe emptyCell $ Map.lookup coord cells


type Cells = Map Coord CellState


getWonCells :: Cells -> [(Player, Coord)]
getWonCells cells =
  [ (fromJust (filledBy cell), coord) | (coord, cell) <- Map.toList cells
                                      , won cell ]

won :: CellState -> Bool
won (CellState l t r b _) = and [l, t, r, b]


placeSegment :: Player -> SegCoord -> Cells -> Cells
placeSegment player (HCoord coord@(x,y)) cells =
  Map.alter (setBot player) (x,y-1) 
  . Map.alter (setTop player) coord
  $ cells
placeSegment player (VCoord coord@(x,y)) cells =
  Map.alter (setRight player) (x-1,y) 
  . Map.alter (setLeft player) coord
  $ cells


setTop :: Player -> Maybe CellState -> Maybe CellState
setTop player cell =
  let cell' = fromMaybe emptyCell cell
  in Just $ cell' { topBorder = True
                  , filledBy = Just player
                  }


setBot :: Player -> Maybe CellState -> Maybe CellState
setBot player cell =
  let cell' = fromMaybe emptyCell cell
  in Just $ cell' { bottomBorder = True
                  , filledBy = Just player
                  }
  

setLeft :: Player -> Maybe CellState -> Maybe CellState
setLeft player cell =
  let cell' = fromMaybe emptyCell cell
  in Just $ cell' { leftBorder = True
                  , filledBy = Just player
                  }


setRight :: Player -> Maybe CellState -> Maybe CellState
setRight player cell =
  let cell' = fromMaybe emptyCell cell
  in Just $ cell' { rightBorder = True
                  , filledBy = Just player
                  }


data CellState
  = CellState
    { leftBorder :: Bool
    , topBorder :: Bool
    , rightBorder :: Bool
    , bottomBorder :: Bool
    , filledBy :: Maybe Player
    }
  deriving (Show, Eq, Ord)


emptyCell :: CellState
emptyCell = CellState False False False False Nothing
