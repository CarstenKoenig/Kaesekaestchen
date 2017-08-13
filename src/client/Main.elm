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
        [ drawSegment 5 ( 0, 50 ) ( 50, 50 )
        , drawSegment 5 ( 50, 0 ) ( 50, 50 )
        , drawSegment 5 ( 50, 50 ) ( 100, 50 )
        , drawSegment 5 ( 50, 50 ) ( 50, 100 )
        ]


drawSegment : Float -> ( Float, Float ) -> ( Float, Float ) -> Svg msg
drawSegment strokeWidth ( x0, y0 ) ( x1, y1 ) =
    let
        sw =
            strokeWidth

        len =
            sqrt ((x1 - x0) ^ 2 + (y1 - y0) ^ 2)

        ( dX, dY ) =
            ( sw * (x1 - x0) / len, sw * (y1 - y0) / len )

        ( oX, oY ) =
            ( dY, -dX )

        showCoord ( x, y ) =
            toString x ++ "," ++ toString y

        pts =
            [ ( x0, y0 )
            , ( x0 + dX + oX, y0 + dY + oY )
            , ( x1 - dX + oX, y1 - dY + oY )
            , ( x1, y1 )
            , ( x1 - dX - oX, y1 - dY - oY )
            , ( x0 + dX - oX, y0 + dY - oY )
            , ( x0, y0 )
            ]
    in
        Svg.polygon
            [ SAttr.fill "black"
            , SAttr.strokeWidth "1"
            , SAttr.stroke "white"
            , SAttr.points (String.join " " (List.map showCoord pts))
            ]
            []
