module Component.Game exposing (Model, Msg, GameId, init, update, view, subscriptions)

import Html exposing (Html)
import Html.Attributes as Attr
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events as SEv
import Svg.Keyed as SKeyed
import Dict exposing (Dict)
import Http
import Api.Game as Api exposing (Player(..), SegCoord(..), GameState)


type alias GameId =
    String


type alias Model =
    { gameId : GameId
    , player : Player
    , hoverOver : Maybe SegCoord
    , takenSegments : Dict SegCoordComp Player
    , wonCells : Dict Coord Player
    , loading : Bool
    , error : Maybe String
    }


type Msg
    = NoOp
    | HoverIn SegCoord
    | HoverOut
    | ClickSeq SegCoord
    | LoadGameResult (Result Http.Error (Maybe GameState))
    | MakeMoveResult (Result Http.Error (Maybe GameState))


type alias Coord =
    ( Int, Int )


type alias SegCoordComp =
    ( Char, Int, Int )


init : GameId -> ( Model, Cmd Msg )
init gameId =
    { gameId = gameId
    , player = Blue
    , hoverOver = Nothing
    , takenSegments = Dict.empty
    , wonCells = Dict.empty
    , error = Nothing
    , loading = True
    }
        ! [ loadGame gameId ]


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        HoverIn coord ->
            { model | hoverOver = Just coord } ! []

        HoverOut ->
            { model | hoverOver = Nothing } ! []

        ClickSeq coord ->
            { model
                | loading = True
            }
                ! [ makeMove model.gameId coord ]

        LoadGameResult (Err error) ->
            -- TODO show error / move away
            { model
                | loading = False
                , error = Just (toString error)
            }
                ! []

        LoadGameResult (Ok gameState) ->
            let
                newModel =
                    setGameState model gameState
            in
                { newModel | loading = False } ! []

        MakeMoveResult (Err error) ->
            -- TODO show error / move away
            { model
                | loading = False
                , error = Just (toString error)
            }
                ! []

        MakeMoveResult (Ok gameState) ->
            let
                newModel =
                    setGameState model gameState
            in
                { newModel | loading = False } ! []


view : Model -> Html Msg
view model =
    Svg.svg
        [ SAttr.viewBox "-5 -5 110 110"
        ]
        (List.concat
            [ fillGrid model
            , drawGrid model
            ]
        )



---------------------------------------------------------------------------------------------------------------


setGameState : Model -> Maybe GameState -> Model
setGameState model state =
    case state of
        Nothing ->
            model

        Just newState ->
            { model
                | player = newState.playersTurn
                , takenSegments =
                    newState.movesMade
                        |> List.map (\( p, sc ) -> ( toComp sc, p ))
                        |> Dict.fromList
                , wonCells =
                    newState.wonCells
                        |> List.map (\( p, c ) -> ( c, p ))
                        |> Dict.fromList
            }


loadGame : GameId -> Cmd Msg
loadGame gameId =
    Http.send LoadGameResult (Api.getApiGameByGameId gameId)


makeMove : GameId -> SegCoord -> Cmd Msg
makeMove gameId coord =
    Http.send MakeMoveResult (Api.postApiGameByGameIdMove gameId coord)


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
                1
                ( 10 * toFloat x, 10 * toFloat y )
                ( 10 * toFloat (x + 1), 10 * toFloat y )

        VCoord ( x, y ) ->
            drawSegmentSvg model
                coord
                1
                ( 10 * toFloat x, 10 * toFloat y )
                ( 10 * toFloat x, 10 * toFloat (y + 1) )


fillRect : Model -> Coord -> Svg Msg
fillRect model ( x, y ) =
    let
        color =
            Dict.get ( x, y ) model.wonCells
                |> Maybe.map playerKey
                |> Maybe.withDefault "white"
    in
        Svg.rect
            [ SAttr.x (toString (x * 10))
            , SAttr.y (toString (y * 10))
            , Attr.height 10
            , Attr.width 10
            , SAttr.fill color
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
            Dict.get (toComp coord) model.takenSegments
                |> Maybe.map playerKey
                |> Maybe.withDefault
                    (if model.hoverOver == Just coord then
                        playerKey model.player
                     else
                        "lightgrey"
                    )

        ( hoverIn, onClick, hoverOut ) =
            if Dict.member (toComp coord) model.takenSegments then
                ( NoOp, NoOp, NoOp )
            else
                ( HoverIn coord, ClickSeq coord, HoverOut )
    in
        SKeyed.node "g"
            []
            [ ( "S_" ++ toString x ++ "_" ++ toString y
              , Svg.polygon
                    [ SAttr.fill color
                    , SAttr.strokeWidth "0.5"
                    , SAttr.stroke "white"
                    , SAttr.points (String.join " " (List.map showCoord pts))
                    , SEv.onMouseOver hoverIn
                    , SEv.onMouseOut hoverOut
                    , SEv.onClick onClick
                    ]
                    []
              )
            ]


toComp : SegCoord -> SegCoordComp
toComp coord =
    case coord of
        VCoord ( x, y ) ->
            ( 'V', x, y )

        HCoord ( x, y ) ->
            ( 'H', x, y )


playerKey : Player -> String
playerKey player =
    case player of
        Red ->
            "red"

        Blue ->
            "blue"
