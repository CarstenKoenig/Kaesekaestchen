module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Svg exposing (Svg)
import Svg.Attributes as SAttr


main : Svg msg
main =
    Svg.svg
        [ SAttr.width "100%"
        , SAttr.height "100%"
        , SAttr.viewBox "0 0 100 100"
        ]
        [ Svg.circle
            [ SAttr.cx "50"
            , SAttr.cy "50"
            , SAttr.r "45"
            , SAttr.style "stroke:blue; fill:red"
            ]
            []
        ]
