module Routing exposing (..)

import Navigation
import UrlParser as Url exposing ((</>))


type Route
    = Services
    | ServicesAdd
    | ServicesEdit Int
    | ServicesConnections Int
    | Connections
    | ConnectionsAdd
    | ConnectionsEdit Int
    | Graph


parse : Navigation.Location -> Maybe Route
parse location =
    Url.parseHash route location


route : Url.Parser (Route -> c) c
route =
    Url.oneOf
        [ Url.map Graph <| Url.top
        , Url.map Services <| Url.s "services"
        , Url.map ServicesAdd <| Url.s "services" </> (Url.s "add")
        , Url.map ServicesEdit <| Url.s "services" </> Url.int
        , Url.map ServicesConnections <| Url.s "services" </> Url.int </> (Url.s "connections")
        , Url.map Connections <| Url.s "connections"
        , Url.map ConnectionsAdd <| Url.s "connections" </> (Url.s "add")
        , Url.map ConnectionsEdit <| Url.s "connections" </> Url.int
        , Url.map Graph <| Url.s "graph"
        ]


getLink : Route -> String
getLink route =
    case route of
        Services ->
            "#/services"

        ServicesAdd ->
            "#/services/add"

        ServicesEdit id ->
            "#/services/" ++ toString id

        ServicesConnections id ->
            "#/services/" ++ toString id ++ "/connections"

        Connections ->
            "#/connections"

        ConnectionsAdd ->
            "#/connections/add"

        ConnectionsEdit id ->
            "#/connections/" ++ toString id

        Graph ->
            "#/graph"


defaultRoute : Route
defaultRoute =
    Services
