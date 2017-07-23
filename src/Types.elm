module Types exposing (..)

import Bootstrap.Navbar as Navbar
import Navigation exposing (Location)
import Pages.Connection
import Pages.Connection.Edit
import Pages.Service
import Pages.Service.Edit
import Pages.Graph
import Routing exposing (Route)


type Msg
    = NewUrl String
    | UrlChange Location
    | NavbarMsg Navbar.State
    | ServicesPageMsg Pages.Service.Msg
    | ConnectionsPageMsg Pages.Connection.Msg
    | EditConnectionPageMsg Pages.Connection.Edit.Msg
    | EditServicePageMsg Pages.Service.Edit.Msg
    | GraphPageMsg Pages.Graph.Msg


type alias Model =
    { currentLocation : Maybe Route
    , history : List Location
    , navbar : Navbar.State
    , subPage : Page
    }


type Page
    = ServicesPage Pages.Service.Model
    | ConnectionsPage Pages.Connection.Model
    | EditConnectionPage Pages.Connection.Edit.Model
    | EditServicePage Pages.Service.Edit.Model
    | GraphPage Pages.Graph.Model
