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
    }


type Msg
    = Refresh
    | LoadGamesResult (Result Http.Error (List GameId))
    | VisitGame GameId


init : ( Model, Cmd Msg )
init =
    { runningGames = []
    , error = Nothing
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
            model ! [ Nav.newUrl (routeToUrl <| PlayGame gameId) ]


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "list-group" ]
        (List.map viewGame model.runningGames)


viewGame : GameId -> Html Msg
viewGame gameId =
    Html.a
        [ Attr.href "#"
        , Attr.class "list-group-item list-group-item-action"
        , Ev.onClick (VisitGame gameId)
        ]
        [ Html.text gameId ]


loadGames : Cmd Msg
loadGames =
    Http.send LoadGamesResult Api.getApiGames
