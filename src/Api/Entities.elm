module Api.Entities exposing (..)

import Json.Decode.Pipeline
import Json.Decode
import Json.Encode


type alias Service =
    { id : Int
    , name :
        String
        -- normalize
    , hostedOn : String
    , description : String
    }


type alias Connection =
    { id : Int
    , name : String
    , from : Int
    , to :
        Int
        -- normalize
    , connectionType : String
    , connectionDetails :
        String
        -- normalize
    , authentication : String
    , description : String
    }


decodeService : Json.Decode.Decoder Service
decodeService =
    Json.Decode.Pipeline.decode Service
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "hostedOn" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)


encodeService : Bool -> Service -> Json.Encode.Value
encodeService withId service =
    let
        descriptor =
            [ ( "name", Json.Encode.string <| service.name )
            , ( "hostedOn", Json.Encode.string <| service.hostedOn )
            , ( "description", Json.Encode.string <| service.description )
            ]
    in
        Json.Encode.object <|
            if withId then
                ( "id", Json.Encode.int service.id ) :: descriptor
            else
                descriptor


decodeConnection : Json.Decode.Decoder Connection
decodeConnection =
    Json.Decode.Pipeline.decode Connection
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "from" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "to" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "connectionType" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "connectionDetails" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "authentication" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)


encodeConnection : Bool -> Connection -> Json.Encode.Value
encodeConnection withId connection =
    let
        descriptor =
            [ ( "name", Json.Encode.string <| connection.name )
            , ( "from", Json.Encode.int <| connection.from )
            , ( "to", Json.Encode.int <| connection.to )
            , ( "connectionType", Json.Encode.string <| connection.connectionType )
            , ( "connectionDetails", Json.Encode.string <| connection.connectionDetails )
            , ( "authentication", Json.Encode.string <| connection.authentication )
            , ( "description", Json.Encode.string <| connection.description )
            ]
    in
        Json.Encode.object <|
            if withId then
                ( "id", Json.Encode.int connection.id ) :: descriptor
            else
                descriptor


emptyService : Service
emptyService =
    Service -1 "" "" ""


emptyConnection : Connection
emptyConnection =
    Connection -1 "" -1 -1 "" "" "" ""
