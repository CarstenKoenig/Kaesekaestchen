module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events as SEv
import Svg.Keyed as SKeyed
import Dict exposing (Dict)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = Model Blue Nothing Dict.empty
        , view = view
        , update = update
        }


type alias Model =
    { player : Color
    , hoverOver : Maybe SegCoord
    , wonCells : Dict Coord Color
    }


type Msg
    = HoverIn SegCoord
    | HoverOut
    | ClickCell Coord


type SegCoord
    = HCoord ( Int, Int )
    | VCoord ( Int, Int )


type Color
    = Blue
    | Red


type alias Coord =
    ( Int, Int )


update : Msg -> Model -> Model
update msg model =
    case msg of
        HoverIn coord ->
            { model | hoverOver = Just coord }

        HoverOut ->
            { model | hoverOver = Nothing }

        ClickCell coord ->
            { model
                | wonCells = Dict.insert coord model.player model.wonCells
                , player = nextColor model.player
            }


nextColor : Color -> Color
nextColor color =
    case color of
        Red ->
            Blue

        Blue ->
            Red


view : Model -> Html Msg
view model =
    Svg.svg
        [ SAttr.width "100%"
        , SAttr.height "100%"
        , SAttr.viewBox "-5 -5 110 110"
        ]
        (List.concat
            [ fillGrid model
            , drawGrid model
            ]
        )


fillGrid : Model -> List (Svg Msg)
fillGrid model =
    List.concatMap (fillRow model) (List.range 0 9)


drawGrid : Model -> List (Svg Msg)
drawGrid model =
    List.concatMap
        (\i -> List.append (drawHLine model i) (drawVLine model i))
        (List.range 0 10)


fillRow : Model -> Int -> List (Svg Msg)
fillRow model y =
    List.map (\x -> fillRect model ( x, y )) (List.range 0 9)


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
                2
                ( 10 * toFloat x, 10 * toFloat y )
                ( 10 * toFloat (x + 1), 10 * toFloat y )

        VCoord ( x, y ) ->
            drawSegmentSvg model
                coord
                2
                ( 10 * toFloat x, 10 * toFloat y )
                ( 10 * toFloat x, 10 * toFloat (y + 1) )


fillRect : Model -> Coord -> Svg Msg
fillRect model ( x, y ) =
    let
        color =
            case Dict.get ( x, y ) model.wonCells of
                Nothing ->
                    "white"

                Just Red ->
                    "red"

                Just Blue ->
                    "blue"
    in
        Svg.rect
            [ SAttr.x (toString (x * 10))
            , SAttr.y (toString (y * 10))
            , Attr.height 10
            , Attr.width 10
            , SAttr.fill color
            , SEv.onClick (ClickCell ( x, y ))
            ]
            []


drawSegmentSvg : Model -> SegCoord -> Float -> ( Float, Float ) -> ( Float, Float ) -> Svg Msg
drawSegmentSvg model coord strokeWidth ( x0, y0 ) ( x1, y1 ) =
    let
        ( x, y ) =
            case coord of
                HCoord c ->
                    c

                VCoord c ->
                    c

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
        SKeyed.node "g"
            []
            [ ( "S_" ++ toString x ++ "_" ++ toString y
              , Svg.polygon
                    [ SAttr.fill color
                    , SAttr.strokeWidth "0.5"
                    , SAttr.stroke "white"
                    , SAttr.points (String.join " " (List.map showCoord pts))
                    , SEv.onMouseOver (HoverIn coord)
                    , SEv.onMouseOut HoverOut
                    ]
                    []
              )
            ]
