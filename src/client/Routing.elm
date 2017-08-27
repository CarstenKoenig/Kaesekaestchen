module Routing exposing (..)

import UrlParser as Url exposing (Parser, (</>))
import Navigation as Nav exposing (Location)
import Component.Game as Game exposing (GameId)


type Route
    = LobbyR
    | ShowGameR GameId


locationToRoute : String -> Location -> Maybe Route
locationToRoute baseUrl =
    Url.parsePath (routeParser baseUrl)


routeToUrl : String -> Route -> String
routeToUrl baseUrl route =
    case route of
        LobbyR ->
            baseUrl

        ShowGameR gameId ->
            "game/" ++ gameId


routeParser : String -> Parser (Route -> a) a
routeParser baseUrl =
    Url.oneOf
        [ Url.map LobbyR (Url.top)
        , Url.map ShowGameR (Url.s "game" </> Url.string)
        ]
