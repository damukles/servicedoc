module Types exposing (..)

import Bootstrap.Navbar as Navbar
import Dict exposing (Dict)
import Http
import Navigation exposing (Location)
import Graph exposing (Graph, NodeId)
import RemoteData exposing (WebData)
import Time exposing (Time)
import Visualization.Force as Force
import Api.Entities exposing (Service, Connection)
import Pages.Service
import Pages.Connection
import Routing exposing (Route)


type Msg
    = Tick Time
    | NewUrl String
    | UrlChange Location
    | NavbarMsg Navbar.State
    | ServicesViewMsg Pages.Service.State
    | ConnectionsViewMsg Pages.Connection.State
    | ResultGetServices (Result Http.Error (List Service))
    | ResultGetConnections (Result Http.Error (List Connection))
    | AddService Service
    | ResultAddService (Result Http.Error Service)
    | UpdateCurrentService Service
    | EditService Service
    | ResultUpdateService (Result Http.Error Service)
    | AddConnection Connection
    | ResultAddConnection (Result Http.Error Connection)
    | EditConnection Connection
    | ResultUpdateConnection (Result Http.Error Connection)
    | UpdateCurrentConnection Connection
    | RefreshGraph


type alias Model =
    { currentLocation : Maybe Route
    , history : List Location
    , navbar : Navbar.State
    , servicesViewState : Pages.Service.State
    , connectionsViewState : Pages.Connection.State
    , currentService : Editable Int Service
    , currentConnection : Editable Int Connection
    , lastAlert : Maybe String
    , services : WebData (Dict Int Service)
    , connections : WebData (Dict Int Connection)
    , graph : Maybe (Graph Entity ())
    , simulation : Maybe (Force.State NodeId)
    }


type Editable i a
    = NotReady i
    | Ready a
    | NotFound
    | NoIntention


type alias Entity =
    Force.Entity NodeId { value : String }
