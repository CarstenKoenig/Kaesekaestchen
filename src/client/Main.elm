module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Navigation as Nav exposing (Location)
import Routing exposing (..)
import Flags exposing (..)
import Component.Game as Game exposing (GameId)
import Component.Lobby as Lobby


main : Program Flags Model Msg
main =
    Nav.programWithFlags
        LocationChanged
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { currentRoute : Route
    , currentView : ViewModel
    , error : Maybe String
    , flags : Flags
    }


type ViewModel
    = ViewLobby Lobby.Model
    | ViewGame Game.Model


type Msg
    = GameMsg Game.Msg
    | LobbyMsg Lobby.Msg
    | LocationChanged Location
    | DismissError


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    case locationToRoute flags.baseUrl location of
        Nothing ->
            let
                ( lobbyModel, lobbyCmd ) =
                    Lobby.init flags.baseUrl
            in
                Model Lobby (ViewLobby lobbyModel) Nothing flags
                    ! [ Nav.modifyUrl <| routeToUrl flags.baseUrl Lobby, Cmd.map LobbyMsg lobbyCmd ]

        Just route ->
            let
                ( viewModel, initCmd ) =
                    viewModelFromRoute flags.baseUrl route
            in
                Model route viewModel Nothing flags
                    ! [ initCmd ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentView of
        ViewLobby lobbyModel ->
            Lobby.subscriptions lobbyModel
                |> Sub.map LobbyMsg

        ViewGame gameModel ->
            Game.subscriptions gameModel
                |> Sub.map GameMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.currentView of
        ViewLobby lobbyModel ->
            updateLobby model msg lobbyModel

        ViewGame gameModel ->
            updateGame model msg gameModel


view : Model -> Html Msg
view model =
    let
        errorView =
            case model.error of
                Nothing ->
                    Html.text ""

                Just error ->
                    viewError error

        bodyView =
            case model.currentView of
                ViewLobby lobbyModel ->
                    Lobby.view lobbyModel
                        |> Html.map LobbyMsg

                ViewGame gameModel ->
                    Game.view gameModel
                        |> Html.map GameMsg
    in
        Html.div []
            [ errorView
            , bodyView
            ]


viewError : String -> Html Msg
viewError error =
    Html.div
        [ Attr.class "alert alert-danger"
        , Attr.attribute "role" "alert"
        , Ev.onClick DismissError
        ]
        [ Html.text error ]


updateLobby : Model -> Msg -> Lobby.Model -> ( Model, Cmd Msg )
updateLobby model msg lobbyModel =
    case msg of
        GameMsg _ ->
            model ! []

        LobbyMsg lobbyMsg ->
            let
                ( newLobbyModel, lobbyCmd ) =
                    Lobby.update lobbyMsg lobbyModel
            in
                { model | currentView = ViewLobby newLobbyModel } ! [ Cmd.map LobbyMsg lobbyCmd ]

        DismissError ->
            { model | error = Nothing } ! []

        LocationChanged loc ->
            handleLocationChanged model loc


updateGame : Model -> Msg -> Game.Model -> ( Model, Cmd Msg )
updateGame model msg gameModel =
    case msg of
        GameMsg gameMsg ->
            let
                ( newGameModel, gameCmd ) =
                    Game.update gameMsg gameModel
            in
                { model | currentView = ViewGame newGameModel } ! [ Cmd.map GameMsg gameCmd ]

        LobbyMsg _ ->
            model ! []

        DismissError ->
            { model | error = Nothing } ! []

        LocationChanged loc ->
            handleLocationChanged model loc


handleLocationChanged : Model -> Location -> ( Model, Cmd Msg )
handleLocationChanged model =
    init model.flags


viewModelFromRoute : String -> Route -> ( ViewModel, Cmd Msg )
viewModelFromRoute baseUrl route =
    case route of
        Lobby ->
            let
                ( lobbyModel, lobbyCmd ) =
                    Lobby.init baseUrl
            in
                ViewLobby lobbyModel ! [ Cmd.map LobbyMsg lobbyCmd ]

        PlayGame gameId ->
            let
                ( gameModel, gameCmd ) =
                    Game.init gameId
            in
                ViewGame gameModel ! [ Cmd.map GameMsg gameCmd ]
