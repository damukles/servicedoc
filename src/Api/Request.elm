module Api.Request exposing (..)

import Http
import Json.Decode
import Api.Entities exposing (..)


apiUrl : String
apiUrl =
    "http://localhost:4000/"


connectionsUrl : String
connectionsUrl =
    apiUrl ++ "connections/"


servicesUrl : String
servicesUrl =
    apiUrl ++ "services/"


getConnections : (Result Http.Error (List Connection) -> msg) -> Cmd msg
getConnections msg =
    Http.get connectionsUrl (Json.Decode.list decodeConnection)
        |> Http.send msg


addConnection : Connection -> (Result Http.Error Connection -> msg) -> Cmd msg
addConnection connection msg =
    Http.post connectionsUrl (Http.jsonBody <| encodeConnection False connection) decodeConnection
        |> Http.send msg


updateConnection : Connection -> (Result Http.Error Connection -> msg) -> Cmd msg
updateConnection connection msg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = connectionsUrl ++ toString connection.id
        , body = Http.jsonBody <| encodeConnection True connection
        , expect = Http.expectJson decodeConnection
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg


getServices : (Result Http.Error (List Service) -> msg) -> Cmd msg
getServices msg =
    Http.get servicesUrl (Json.Decode.list decodeService)
        |> Http.send msg


addService : Service -> (Result Http.Error Service -> msg) -> Cmd msg
addService service msg =
    Http.post servicesUrl (Http.jsonBody <| encodeService False service) decodeService
        |> Http.send msg


updateService : Service -> (Result Http.Error Service -> msg) -> Cmd msg
updateService service msg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = servicesUrl ++ toString service.id
        , body = Http.jsonBody <| encodeService True service
        , expect = Http.expectJson decodeService
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg
