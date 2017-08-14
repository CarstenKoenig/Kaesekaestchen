{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game
  ( Player
  , Coord
  , writeElmFile
  ) where

import GHC.Generics (Generic)
import           Elm (ElmType(..), Spec(Spec))
import qualified Elm
import           Servant.Elm (Proxy(Proxy))
import qualified Servant.Elm as Elm


data Player
  = Blue | Red
  deriving (Show, Eq, Ord, Enum, Generic)

instance ElmType Player
  

data SegCoord
  = HCoord Coord
  | VCoord Coord
  deriving (Show, Eq, Ord, Generic)

instance ElmType SegCoord


type Coord = (Int, Int)


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
  ]
