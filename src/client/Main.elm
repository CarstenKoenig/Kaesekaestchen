module Main exposing (..)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events as SEv


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = Model Nothing
        , view = view
        , update = update
        }


type alias Model =
    { hoverOver : Maybe SegCoord
    }


type Msg
    = HoverIn SegCoord
    | HoverOut


type SegCoord
    = HCoord ( Int, Int )
    | VCoord ( Int, Int )


update : Msg -> Model -> Model
update msg model =
    case msg of
        HoverIn coord ->
            { model | hoverOver = Just coord }

        HoverOut ->
            { model | hoverOver = Nothing }


view : Model -> Html Msg
view model =
    Svg.svg
        [ SAttr.width "100%"
        , SAttr.height "100%"
        , SAttr.viewBox "-5 -5 110 110"
        ]
        (drawGrid model)


drawGrid : Model -> List (Svg Msg)
drawGrid model =
    List.concatMap
        (\i -> List.append (drawHLine model i) (drawVLine model i))
        (List.range 0 10)


drawHLine : Model -> Int -> List (Svg Msg)
drawHLine model y =
    List.map (\x -> drawSegment model (HCoord ( x, y ))) (List.range 0 9)


drawVLine : Model -> Int -> List (Svg Msg)
drawVLine model x =
    List.map (\y -> drawSegment model (VCoord ( x, y ))) (List.range 0 9)


drawSegment : Model -> SegCoord -> Svg Msg
drawSegment model coord =
    case coord of
        HCoord ( x, y ) ->
            drawSegmentSvg model
                coord
                1
                ( 10 * toFloat x, 10 * toFloat y )
                ( 10 * toFloat (x + 1), 10 * toFloat y )

        VCoord ( x, y ) ->
            drawSegmentSvg model
                coord
                1
                ( 10 * toFloat x, 10 * toFloat y )
                ( 10 * toFloat x, 10 * toFloat (y + 1) )


drawSegmentSvg : Model -> SegCoord -> Float -> ( Float, Float ) -> ( Float, Float ) -> Svg Msg
drawSegmentSvg model coord strokeWidth ( x0, y0 ) ( x1, y1 ) =
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

        color =
            if model.hoverOver == Just coord then
                "blue"
            else
                "black"
    in
        Svg.polygon
            [ SAttr.fill color
            , SAttr.strokeWidth "0.5"
            , SAttr.stroke "white"
            , SAttr.points (String.join " " (List.map showCoord pts))
            , SEv.onMouseOver (HoverIn coord)
            , SEv.onMouseOut HoverOut
            ]
            []
