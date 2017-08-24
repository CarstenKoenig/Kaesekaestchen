module Routing exposing (..)

import UrlParser as Url exposing (Parser, (</>))
import Navigation as Nav exposing (Location)
import Component.Game as Game exposing (GameId)


type Route
    = Lobby
    | PlayGame GameId


locationToRoute : Location -> Maybe Route
locationToRoute =
    Url.parsePath routeParser


routeToUrl : Route -> String
routeToUrl route =
    case route of
        Lobby ->
            "/"

        PlayGame gameId ->
            "/game/" ++ gameId


routeParser : Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map Lobby (Url.s "")
        , Url.map PlayGame (Url.s "game" </> Url.string)
        ]
