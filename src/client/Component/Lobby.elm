module Component.Lobby exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Navigation as Nav
import Routing exposing (..)
import Api.Game as Api exposing (Player(..), SegCoord(..), GameState)
import Component.Game exposing (GameId)


type alias Model =
    { runningGames : List GameId
    , error : Maybe String
    , baseUrl : String
    }


type Msg
    = Refresh
    | LoadGamesResult (Result Http.Error (List GameId))
    | VisitGame GameId
    | StartNewGame
    | StartNewGameResult (Result Http.Error GameId)


init : String -> ( Model, Cmd Msg )
init baseUrl =
    { runningGames = []
    , error = Nothing
    , baseUrl = baseUrl
    }
        ! [ loadGames ]


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            { model
                | runningGames = []
                , error = Nothing
            }
                ! [ loadGames ]

        LoadGamesResult (Err error) ->
            -- TODO show error / move away
            { model
                | error = Just (toString error)
            }
                ! []

        LoadGamesResult (Ok games) ->
            { model | runningGames = games } ! []

        VisitGame gameId ->
            model ! [ Nav.newUrl (routeToUrl model.baseUrl <| PlayGame gameId) ]

        StartNewGame ->
            model ! [ startNewGame ]

        StartNewGameResult (Err error) ->
            -- TODO show error / move away
            { model
                | error = Just (toString error)
            }
                ! []

        StartNewGameResult (Ok gameId) ->
            model ! [ Nav.newUrl (routeToUrl model.baseUrl <| PlayGame gameId) ]


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "container" ]
        [ Html.div
            [ Attr.class "row" ]
            [ Html.h1 [] [ Html.text "Lobby" ] ]
        , Html.div
            [ Attr.class "row" ]
            [ Html.div
                [ Attr.class "list-group" ]
                (List.map viewGame model.runningGames)
            ]
        , Html.div
            [ Attr.class "row" ]
            [ Html.div
                [ Attr.class "btn-group", Attr.attribute "role" "group", Attr.attribute "aria-label" "Basic example" ]
                [ Html.button
                    [ Attr.type_ "button"
                    , Attr.class "btn btn-secondary"
                    , Ev.onClick StartNewGame
                    ]
                    [ Html.text "New" ]
                ]
            ]
        ]


viewGame : GameId -> Html Msg
viewGame gameId =
    Html.button
        [ Attr.class "list-group-item list-group-item-action"
        , Ev.onClick (VisitGame gameId)
        ]
        [ Html.text gameId ]


loadGames : Cmd Msg
loadGames =
    Http.send LoadGamesResult Api.getApiGames


startNewGame : Cmd Msg
startNewGame =
    Http.send StartNewGameResult Api.postApiGameNew
