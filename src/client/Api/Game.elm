module Api.Game exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Exts.Json.Encode


getApiGames : Http.Request (List String)
getApiGames =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "api"
                , "games"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list string)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiGameByGameId : String -> Http.Request (Maybe GameState)
getApiGameByGameId capture_gameId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "api"
                , "game"
                , capture_gameId |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (nullable decodeGameState)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postApiGameNewByDim : Int -> Http.Request String
postApiGameNewByDim capture_dim =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "api"
                , "game"
                , "new"
                , capture_dim |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postApiGameByGameIdMove : String -> SegCoord -> Http.Request (Maybe GameState)
postApiGameByGameIdMove capture_gameId body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "api"
                , "game"
                , capture_gameId |> Http.encodeUri
                , "move"
                ]
        , body =
            Http.jsonBody (encodeSegCoord body)
        , expect =
            Http.expectJson (nullable decodeGameState)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


type Player
    = Blue
    | Red


decodePlayer : Decoder Player
decodePlayer =
    string
        |> andThen
            (\x ->
                if x == "Blue" then
                    decode Blue
                else if x == "Red" then
                    decode Red
                else
                    fail "Constructor not matched"
            )


type SegCoord
    = HCoord ( Int, Int )
    | VCoord ( Int, Int )


decodeSegCoord : Decoder SegCoord
decodeSegCoord =
    field "tag" string
        |> andThen
            (\x ->
                if x == "HCoord" then
                    decode HCoord |> required "contents" (map2 (,) (index 0 int) (index 1 int))
                else if x == "VCoord" then
                    decode VCoord |> required "contents" (map2 (,) (index 0 int) (index 1 int))
                else
                    fail "Constructor not matched"
            )


encodeSegCoord : SegCoord -> Json.Encode.Value
encodeSegCoord x =
    case x of
        HCoord y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "HCoord" )
                , ( "contents", (Exts.Json.Encode.tuple2 Json.Encode.int Json.Encode.int) y0 )
                ]

        VCoord y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "VCoord" )
                , ( "contents", (Exts.Json.Encode.tuple2 Json.Encode.int Json.Encode.int) y0 )
                ]


type SegmentFill
    = Wall
    | Color Player


decodeSegmentFill : Decoder SegmentFill
decodeSegmentFill =
    field "tag" string
        |> andThen
            (\x ->
                if x == "Wall" then
                    decode Wall
                else if x == "Color" then
                    decode Color |> required "contents" decodePlayer
                else
                    fail "Constructor not matched"
            )


type alias GameState =
    { dimension : Int
    , filledSegments : List ( SegmentFill, SegCoord )
    , wonCells : List ( Player, ( Int, Int ) )
    , playersTurn : Player
    }


decodeGameState : Decoder GameState
decodeGameState =
    decode GameState
        |> required "dimension" int
        |> required "filledSegments" (list (map2 (,) (index 0 decodeSegmentFill) (index 1 decodeSegCoord)))
        |> required "wonCells" (list (map2 (,) (index 0 decodePlayer) (index 1 (map2 (,) (index 0 int) (index 1 int)))))
        |> required "playersTurn" decodePlayer
