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
import Pages.Connection.Edit
import Routing exposing (Route)


type Msg
    = Tick Time
    | NewUrl String
    | UrlChange Location
    | NavbarMsg Navbar.State
    | EditConnectionPageMsg Pages.Connection.Edit.Msg
    | ServicesViewMsg Pages.Service.State
    | ConnectionsViewMsg Pages.Connection.State
    | ResultGetServices (Result Http.Error (List Service))
    | ResultGetConnections (Result Http.Error (List Connection))
    | AddService Service
    | ResultAddService (Result Http.Error Service)
    | UpdateCurrentService Service
    | EditService Service
    | ResultUpdateService (Result Http.Error Service)
    | RefreshGraph


type alias Model =
    { currentLocation : Maybe Route
    , history : List Location
    , navbar : Navbar.State
    , servicesViewState : Pages.Service.State
    , connectionsViewState : Pages.Connection.State
    , currentService : Editable Int Service
    , subPage : Page
    , lastAlert : Maybe String
    , services : WebData (Dict Int Service)
    , connections : WebData (Dict Int Connection)
    , graph : Maybe (Graph Entity ())
    , simulation : Maybe (Force.State NodeId)
    }


type Page
    = None
    | EditConnectionPage Pages.Connection.Edit.Model


type Editable i a
    = NotReady i
    | Ready a
    | NotFound
    | NoIntention


type alias Entity =
    Force.Entity NodeId { value : String }
