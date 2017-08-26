{-# LANGUAGE OverloadedStrings #-}

module ElmExport
  ( writeElmFile
  , toJson
  ) where

import           Data.Aeson (encode)
import           Data.ByteString.Lazy (ByteString)
import           Elm (Spec(Spec))
import qualified Elm
import           Servant.Elm (Proxy(Proxy))
import qualified Servant.Elm as Elm


import Game
import Lib


toJson :: GameState -> ByteString
toJson = encode


writeElmFile :: FilePath -> IO ()
writeElmFile = Elm.specsToDir [spec]


spec :: Spec
spec = Spec
  ["Api", "Game"]
  ( Elm.defElmImports
  : "import Exts.Json.Encode"
  : Elm.generateElmForAPIWith
    (Elm.defElmOptions { Elm.urlPrefix = Elm.Dynamic })
    (Proxy :: Proxy API)
  ++
  [ Elm.toElmTypeSource    (Proxy :: Proxy Player)
  , Elm.toElmDecoderSource (Proxy :: Proxy Player)
  , Elm.toElmTypeSource    (Proxy :: Proxy SegCoord)
  , Elm.toElmDecoderSource (Proxy :: Proxy SegCoord)
  , Elm.toElmEncoderSource (Proxy :: Proxy SegCoord)
  , Elm.toElmTypeSource    (Proxy :: Proxy SegmentFill)
  , Elm.toElmDecoderSource (Proxy :: Proxy SegmentFill)
  , Elm.toElmTypeSource    (Proxy :: Proxy GameState)
  , Elm.toElmDecoderSource (Proxy :: Proxy GameState)
  , Elm.toElmTypeSource    (Proxy :: Proxy GameResponse)
  , Elm.toElmDecoderSource (Proxy :: Proxy GameResponse)
  ])
