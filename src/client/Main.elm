module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import UrlParser as Url exposing (Parser, (</>))
import Navigation as Nav exposing (Location)
import Component.Game as Game exposing (GameId)
import Api.Game as Api exposing (Player(..), SegCoord(..), GameState)


main : Program Never Model Msg
main =
    Nav.program
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
    }


type ViewModel
    = NoContent
    | InGame Game.Model


type Route
    = Home
    | PlayGame GameId


type Msg
    = InGameMsg Game.Msg
    | StartNewGameResult (Result Http.Error GameId)
    | LocationChanged Location
    | DismissError


init : Location -> ( Model, Cmd Msg )
init location =
    case locationToRoute location of
        Nothing ->
            Model Home NoContent Nothing ! [ Nav.modifyUrl <| routeToUrl Home ]

        Just route ->
            let
                ( viewModel, initCmd ) =
                    viewModelFromRoute route
            in
                Model route viewModel Nothing ! [ initCmd ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentView of
        NoContent ->
            Sub.none

        InGame gameModel ->
            Game.subscriptions gameModel
                |> Sub.map InGameMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.currentView of
        NoContent ->
            updateNoContent model msg

        InGame gameModel ->
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
                NoContent ->
                    Html.text ""

                InGame gameModel ->
                    Game.view gameModel
                        |> Html.map InGameMsg
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


updateNoContent : Model -> Msg -> ( Model, Cmd Msg )
updateNoContent model msg =
    case msg of
        InGameMsg _ ->
            { model | currentView = NoContent } ! []

        StartNewGameResult result ->
            handleStartNewGameResult model result

        DismissError ->
            { model | error = Nothing } ! []

        LocationChanged loc ->
            handleLocationChanged model loc


updateGame : Model -> Msg -> Game.Model -> ( Model, Cmd Msg )
updateGame model msg gameModel =
    case msg of
        InGameMsg gameMsg ->
            let
                ( newGameModel, gameCmd ) =
                    Game.update gameMsg gameModel
            in
                { model | currentView = InGame newGameModel } ! [ Cmd.map InGameMsg gameCmd ]

        StartNewGameResult result ->
            handleStartNewGameResult model result

        DismissError ->
            { model | error = Nothing } ! []

        LocationChanged loc ->
            handleLocationChanged model loc


handleLocationChanged : Model -> Location -> ( Model, Cmd Msg )
handleLocationChanged model loc =
    case locationToRoute loc of
        Just route ->
            { model | currentRoute = route } ! []

        Nothing ->
            init loc


handleStartNewGameResult : Model -> Result Http.Error GameId -> ( Model, Cmd Msg )
handleStartNewGameResult model result =
    case result of
        Err error ->
            { model
                | error = Just (toString error)
                , currentView = NoContent
            }
                ! []

        Ok gameId ->
            let
                ( gameModel, gameCmd ) =
                    Game.init gameId
            in
                { model
                    | currentView = InGame gameModel
                }
                    ! [ Cmd.map InGameMsg gameCmd ]


startNewGame : Cmd Msg
startNewGame =
    Http.send StartNewGameResult Api.postApiGameNew


viewModelFromRoute : Route -> ( ViewModel, Cmd Msg )
viewModelFromRoute route =
    case route of
        Home ->
            NoContent ! []

        PlayGame gameId ->
            let
                ( gameModel, gameCmd ) =
                    Game.init gameId
            in
                InGame gameModel ! [ Cmd.map InGameMsg gameCmd ]


locationToRoute : Location -> Maybe Route
locationToRoute =
    Url.parsePath routeParser


routeToUrl : Route -> String
routeToUrl route =
    case route of
        Home ->
            "/"

        PlayGame gameId ->
            "/game/" ++ gameId


routeParser : Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map Home (Url.s "")
        , Url.map PlayGame (Url.s "game" </> Url.string)
        ]
