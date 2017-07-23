module Types exposing (..)

import Api.Entities exposing (Connection, Service)
import Bootstrap.Navbar as Navbar
import Dict exposing (Dict)
import Graph exposing (Graph, NodeId)
import Http
import Navigation exposing (Location)
import Pages.Connection
import Pages.Connection.Edit
import Pages.Service
import Pages.Service.Edit
import RemoteData exposing (WebData)
import Routing exposing (Route)
import Time exposing (Time)
import Visualization.Force as Force


type Msg
    = Tick Time
    | NewUrl String
    | UrlChange Location
    | NavbarMsg Navbar.State
    | EditConnectionPageMsg Pages.Connection.Edit.Msg
    | EditServicePageMsg Pages.Service.Edit.Msg
    | ServicesViewMsg Pages.Service.State
    | ConnectionsViewMsg Pages.Connection.State
    | ResultGetServices (Result Http.Error (List Service))
    | ResultGetConnections (Result Http.Error (List Connection))
    | RefreshGraph


type alias Model =
    { currentLocation : Maybe Route
    , history : List Location
    , navbar : Navbar.State
    , servicesViewState : Pages.Service.State
    , connectionsViewState : Pages.Connection.State
    , subPage : Page
    , services : WebData (Dict Int Service)
    , connections : WebData (Dict Int Connection)
    , graph : Maybe (Graph Entity ())
    , simulation : Maybe (Force.State NodeId)
    }


type Page
    = None
    | EditConnectionPage Pages.Connection.Edit.Model
    | EditServicePage Pages.Service.Edit.Model


type alias Entity =
    Force.Entity NodeId { value : String }
