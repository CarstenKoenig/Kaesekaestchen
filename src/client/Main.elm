module Main exposing (..)

import Html exposing (Html)
import Http
import Component.Game as Game exposing (GameId)
import Api.Game as Api exposing (Player(..), SegCoord(..), GameState)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


type Model
    = NoContent
    | InGame Game.Model


type Msg
    = InGameMsg Game.Msg
    | StartNewGameResult (Result Http.Error GameId)


init : ( Model, Cmd Msg )
init =
    NoContent ! [ startNewGame ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NoContent ->
            Sub.none

        InGame gameModel ->
            Game.subscriptions gameModel
                |> Sub.map InGameMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NoContent ->
            updateNoContent msg

        InGame model ->
            updateGame msg model


view : Model -> Html Msg
view model =
    case model of
        NoContent ->
            Html.div [] []

        InGame gameModel ->
            Game.view gameModel
                |> Html.map InGameMsg


updateNoContent : Msg -> ( Model, Cmd Msg )
updateNoContent msg =
    case msg of
        InGameMsg _ ->
            NoContent ! []

        StartNewGameResult result ->
            handleStartNewGameResult result


updateGame : Msg -> Game.Model -> ( Model, Cmd Msg )
updateGame msg model =
    case msg of
        InGameMsg gameMsg ->
            let
                ( newGameModel, gameCmd ) =
                    Game.update gameMsg model
            in
                InGame newGameModel ! [ Cmd.map InGameMsg gameCmd ]

        StartNewGameResult result ->
            handleStartNewGameResult result


handleStartNewGameResult : Result Http.Error GameId -> ( Model, Cmd Msg )
handleStartNewGameResult result =
    case result of
        Err error ->
            -- TODO: show some error
            NoContent ! []

        Ok gameId ->
            let
                ( gameModel, gameCmd ) =
                    Game.init gameId
            in
                ( InGame gameModel, Cmd.map InGameMsg gameCmd )


startNewGame : Cmd Msg
startNewGame =
    Http.send StartNewGameResult Api.postApiGameNew
