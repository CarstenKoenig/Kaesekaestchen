module Routing exposing (..)

import UrlParser as Url exposing (Parser, (</>))
import Navigation as Nav exposing (Location)
import Component.Game as Game exposing (GameId)


type Route
    = Lobby
    | PlayGame GameId


locationToRoute : String -> Location -> Maybe Route
locationToRoute baseUrl =
    Url.parsePath (routeParser baseUrl)


routeToUrl : String -> Route -> String
routeToUrl baseUrl route =
    case route of
        Lobby ->
            baseUrl

        PlayGame gameId ->
            baseUrl ++ "/game/" ++ gameId


routeParser : String -> Parser (Route -> a) a
routeParser baseUrl =
    Url.oneOf
        [ Url.map Lobby (Url.s baseUrl)
        , Url.map PlayGame (Url.s baseUrl </> Url.s "game" </> Url.string)
        ]
