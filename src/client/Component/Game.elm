module Component.Game exposing (Model, Msg, GameId, init, update, view, subscriptions)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Json.Decode as Json
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events as SEv
import Svg.Keyed as SKeyed
import Dict exposing (Dict)
import Http
import WebSocket as WS
import Flags exposing (Flags)
import Api.Game as Api exposing (Player(..), SegCoord(..), SegmentFill(..), GameState, GameResponse, decodeGameResponse)


type alias GameId =
    String


type alias Model =
    { flags : Flags
    , gameId : GameId
    , dimension : Int
    , player : Player
    , hoverOver : Maybe SegCoord
    , takenSegments : Dict SegCoordComp SegmentFill
    , wonCells : Dict Coord Player
    , myTurn : Bool
    , loading : Bool
    , error : Maybe String
    }


type Msg
    = NoOp
    | HoverIn SegCoord
    | HoverOut
    | ClickSeq SegCoord
    | Refresh
    | ReceivedUpdate GameResponse
    | LoadGameResult (Result Http.Error (Maybe GameResponse))
    | MakeMoveResult (Result Http.Error (Maybe GameResponse))


type alias Coord =
    ( Int, Int )


type alias SegCoordComp =
    ( Char, Int, Int )


init : Flags -> Int -> GameId -> ( Model, Cmd Msg )
init flags dim gameId =
    { flags = flags
    , gameId = gameId
    , dimension = dim
    , player = Blue
    , hoverOver = Nothing
    , takenSegments = Dict.empty
    , wonCells = Dict.empty
    , myTurn = False
    , error = Nothing
    , loading = True
    }
        ! [ joinGame flags.apiUrl gameId ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        url =
            "wss://" ++ String.dropLeft (String.length "https://") model.flags.apiUrl ++ "/api/game/" ++ model.gameId ++ "/subscribe"

        decode s =
            case Json.decodeString decodeGameResponse s of
                Err _ ->
                    NoOp

                Ok res ->
                    ReceivedUpdate res
    in
        WS.listen url decode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Refresh ->
            model ! [ loadGame model.flags.apiUrl model.gameId ]

        HoverIn coord ->
            if model.myTurn then
                { model | hoverOver = Just coord } ! []
            else
                { model | hoverOver = Nothing } ! []

        HoverOut ->
            { model | hoverOver = Nothing } ! []

        ClickSeq coord ->
            if model.myTurn then
                { model
                    | loading = True
                }
                    ! [ makeMove model.flags.apiUrl model.gameId coord ]
            else
                model ! []

        LoadGameResult (Err error) ->
            -- TODO show error / move away
            { model
                | loading = False
                , myTurn = False
                , error = Just (toString error)
            }
                ! []

        LoadGameResult (Ok response) ->
            let
                newModel =
                    setGameState model response
            in
                { newModel | loading = False } ! []

        ReceivedUpdate response ->
            setGameState model (Just response) ! []

        MakeMoveResult (Err error) ->
            -- TODO show error / move away
            { model
                | loading = False
                , hoverOver = Nothing
                , myTurn = False
                , error = Just (toString error)
            }
                ! []

        MakeMoveResult (Ok gameState) ->
            let
                newModel =
                    setGameState model gameState
            in
                { newModel
                    | loading = False
                    , hoverOver = Nothing
                }
                    ! []


view : Model -> Html Msg
view model =
    let
        width =
            model.dimension * 10 + 10
    in
        Html.div
            [ Attr.class "container" ]
            [ Html.div
                [ Attr.class "row" ]
                [ Svg.svg
                    [ SAttr.viewBox <| "-5 -5 " ++ toString width ++ " " ++ toString width
                    ]
                    (List.concat
                        [ fillGrid model
                        , drawGrid model
                        ]
                    )
                ]
            , Html.div
                [ Attr.class "row" ]
                [ Html.button
                    [ Attr.class "btn btn-default"
                    , Ev.onClick Refresh
                    ]
                    [ Html.text "refresh" ]
                ]
            ]



---------------------------------------------------------------------------------------------------------------


setGameState : Model -> Maybe GameResponse -> Model
setGameState model response =
    case response of
        Nothing ->
            model

        Just newResponse ->
            { model
                | player = newResponse.gameState.playersTurn
                , dimension = newResponse.gameState.dimension
                , myTurn = newResponse.yourMove
                , takenSegments =
                    newResponse.gameState.filledSegments
                        |> List.map (\( p, sc ) -> ( toComp sc, p ))
                        |> Dict.fromList
                , wonCells =
                    newResponse.gameState.wonCells
                        |> List.map (\( p, c ) -> ( c, p ))
                        |> Dict.fromList
            }


loadGame : String -> GameId -> Cmd Msg
loadGame apiUrl gameId =
    Http.send LoadGameResult (Api.getApiGameByGameId apiUrl gameId)


joinGame : String -> GameId -> Cmd Msg
joinGame apiUrl gameId =
    Http.send LoadGameResult (Api.postApiGameByGameIdJoin apiUrl gameId)


makeMove : String -> GameId -> SegCoord -> Cmd Msg
makeMove apiUrl gameId coord =
    Http.send MakeMoveResult (Api.postApiGameByGameIdMove apiUrl gameId coord)


fillGrid : Model -> List (Svg Msg)
fillGrid model =
    List.concatMap (fillRow model) (List.range 0 (model.dimension - 1))


drawGrid : Model -> List (Svg Msg)
drawGrid model =
    List.concatMap
        (\i -> List.append (drawHLine model i) (drawVLine model i))
        (List.range 0 model.dimension)


fillRow : Model -> Int -> List (Svg Msg)
fillRow model y =
    List.map (\x -> fillRect model ( x, y )) (List.range 0 (model.dimension - 1))


drawHLine : Model -> Int -> List (Svg Msg)
drawHLine model y =
    List.map (\x -> drawSegment model (HCoord ( x, y ))) (List.range 0 (model.dimension - 1))


drawVLine : Model -> Int -> List (Svg Msg)
drawVLine model x =
    List.map (\y -> drawSegment model (VCoord ( x, y ))) (List.range 0 (model.dimension - 1))


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
                |> Maybe.map segmentFillKey
                |> Maybe.withDefault
                    (if model.hoverOver == Just coord then
                        playerKey model.player
                     else
                        "lightgrey"
                    )

        attributes =
            if not model.myTurn || Dict.member (toComp coord) model.takenSegments then
                []
            else
                [ SEv.onMouseOver (HoverIn coord)
                , SEv.onClick (ClickSeq coord)
                , SEv.onMouseOut HoverOut
                ]
    in
        SKeyed.node "g"
            []
            [ ( "S_" ++ toString x ++ "_" ++ toString y
              , Svg.polygon
                    ([ SAttr.fill color
                     , SAttr.strokeWidth "0.5"
                     , SAttr.stroke "white"
                     , SAttr.points (String.join " " (List.map showCoord pts))
                     ]
                        ++ attributes
                    )
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


segmentFillKey : SegmentFill -> String
segmentFillKey seg =
    case seg of
        Color pl ->
            playerKey pl

        Wall ->
            "grey"


playerKey : Player -> String
playerKey pl =
    case pl of
        Red ->
            "red"

        Blue ->
            "blue"
