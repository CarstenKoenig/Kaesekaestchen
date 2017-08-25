{-# LANGUAGE OverloadedStrings #-}

module ElmExport
  ( writeElmFile
  , toJson
  ) where

import           Data.Aeson (encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import           Elm (Spec(Spec))
import qualified Elm
import           Servant.Elm (Proxy(Proxy))
import qualified Servant.Elm as Elm


import Game
import Lib


toJson :: GameState -> ByteString
toJson = encode


writeElmFile :: Text -> FilePath -> IO ()
writeElmFile baseUrl = Elm.specsToDir [spec baseUrl]


spec :: Text -> Spec
spec baseUrl = Spec
  ["Api", "Game"]
  ( Elm.defElmImports
  : "import Exts.Json.Encode"
  : Elm.generateElmForAPIWith
    (Elm.defElmOptions { Elm.urlPrefix = Elm.Static baseUrl })
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
  ])
