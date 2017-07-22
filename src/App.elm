module App exposing (init, update, subscriptions)

import AnimationFrame
import Api.Entities exposing (Connection, Service)
import Api.Request as Api
import Bootstrap.Navbar as Navbar
import Dict exposing (Dict)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Navigation exposing (Location)
import Pages.Connection
import Pages.Graph
import Pages.Service
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..))
import Types exposing (..)
import Visualization.Force as Force exposing (State)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        ( currentLocation, currentService, currentConnection ) =
            parseAndPrepareRoute NotAsked NotAsked location
    in
        { currentLocation = currentLocation
        , history = []
        , navbar = navbarState
        , servicesViewState = Pages.Service.init
        , connectionsViewState = Pages.Connection.init
        , services = NotAsked
        , connections = NotAsked
        , currentService = currentService
        , currentConnection = currentConnection
        , lastAlert = Nothing
        , graph = Nothing
        , simulation = Nothing
        }
            ! [ navbarCmd
              , Api.getServices ResultGetServices
              , Api.getConnections ResultGetConnections
              ]


emptyService : Service
emptyService =
    Service -1 "" "" ""


emptyConnection : Connection
emptyConnection =
    Connection -1 "" -1 -1 "" "" "" ""


oops : String
oops =
    "Oops, something went wrong there."


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ graph, simulation } as model) =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            let
                ( currentLocation, currentService, currentConnection ) =
                    parseAndPrepareRoute model.services model.connections location
            in
                { model
                    | history = location :: model.history
                    , currentLocation = currentLocation
                    , currentService = currentService
                    , currentConnection = currentConnection
                }
                    ! []

        NavbarMsg state ->
            { model | navbar = state } ! []

        ServicesViewMsg state ->
            { model | servicesViewState = state } ! []

        ConnectionsViewMsg state ->
            { model | connectionsViewState = state } ! []

        Tick t ->
            case ( graph, simulation ) of
                ( Just realGraph, Just realSimulation ) ->
                    let
                        ( simulation_, list ) =
                            Force.tick realSimulation <| List.map .label <| Graph.nodes realGraph

                        graph_ =
                            (Pages.Graph.updateGraphWithList realGraph list)
                    in
                        { model | graph = Just graph_, simulation = Just simulation_ } ! []

                _ ->
                    model ! []

        ResultGetServices (Ok services) ->
            let
                services_ =
                    Success <| Dict.fromList <| List.map (\x -> ( x.id, x )) services

                ( graph_, simulation_ ) =
                    updateGraphAndSim services_ model.connections
            in
                { model
                    | services = services_
                    , graph = graph_
                    , simulation = simulation_
                }
                    ! []

        ResultGetServices (Err err) ->
            { model | services = Failure err } ! []

        ResultGetConnections (Ok connections) ->
            let
                connections_ =
                    Success <| Dict.fromList <| List.map (\x -> ( x.id, x )) connections

                ( graph_, simulation_ ) =
                    updateGraphAndSim model.services connections_
            in
                { model
                    | connections = connections_
                    , graph = graph_
                    , simulation = simulation_
                }
                    ! []

        ResultGetConnections (Err err) ->
            { model | connections = Failure err } ! []

        AddService service ->
            model ! [ Api.addService service ResultAddService ]

        ResultAddService (Ok service) ->
            let
                ( services_, cmd ) =
                    case model.services of
                        Success data ->
                            ( Success <| Dict.insert service.id service data, Cmd.none )

                        _ ->
                            ( model.services, Api.getServices ResultGetServices )
            in
                { model | services = services_, lastAlert = Nothing } ! [ cmd, Navigation.back 1 ]

        ResultAddService (Err err) ->
            let
                out =
                    Debug.log "ResultAddService Error" <| toString err
            in
                { model | lastAlert = Just oops } ! []

        UpdateCurrentService service ->
            { model | currentService = Just service } ! []

        EditService service ->
            model ! [ Api.updateService service ResultUpdateService ]

        ResultUpdateService (Ok service) ->
            let
                services_ =
                    RemoteData.map (Dict.insert service.id service) model.services
            in
                { model | services = services_, lastAlert = Nothing } ! [ Navigation.back 1 ]

        ResultUpdateService (Err err) ->
            let
                out =
                    Debug.log "ResultUpdateService Error" <| toString err
            in
                { model | lastAlert = Just oops } ! []

        AddConnection connection ->
            model ! [ Api.addConnection connection ResultAddConnection ]

        ResultAddConnection (Ok connection) ->
            let
                ( connections_, cmd ) =
                    case model.connections of
                        Success data ->
                            ( Success <| Dict.insert connection.id connection data, Cmd.none )

                        _ ->
                            ( model.connections, Api.getConnections ResultGetConnections )
            in
                { model | connections = connections_, lastAlert = Nothing } ! [ cmd, Navigation.back 1 ]

        ResultAddConnection (Err err) ->
            let
                out =
                    Debug.log "ResultAddConnection Error" <| toString err
            in
                { model | lastAlert = Just oops } ! []

        UpdateCurrentConnection connection ->
            { model | currentConnection = Just connection } ! []

        EditConnection connection ->
            model ! [ Api.updateConnection connection ResultUpdateConnection ]

        ResultUpdateConnection (Ok connection) ->
            let
                connections_ =
                    RemoteData.map (Dict.insert connection.id connection) model.connections
            in
                { model | connections = connections_, lastAlert = Nothing } ! [ Navigation.back 1 ]

        ResultUpdateConnection (Err err) ->
            let
                out =
                    Debug.log "ResultUpdateConnection Error" <| toString err
            in
                { model | lastAlert = Just oops } ! []

        RefreshGraph ->
            let
                ( graph, simulation ) =
                    updateGraphAndSim model.services model.connections
            in
                { model
                    | graph = graph
                    , simulation = simulation
                }
                    ! []


parseAndPrepareRoute : WebData (Dict Int Service) -> WebData (Dict Int Connection) -> Location -> ( Maybe Route, Maybe Service, Maybe Connection )
parseAndPrepareRoute services connections location =
    let
        currentLocation =
            Routing.parse location

        ( currentService, currentConnection ) =
            case currentLocation of
                Just ServicesAdd ->
                    ( Just emptyService, Nothing )

                Just (ServicesEdit id) ->
                    ( findById id services, Nothing )

                Just ConnectionsAdd ->
                    ( Nothing, Just emptyConnection )

                Just (ConnectionsEdit id) ->
                    ( Nothing, findById id connections )

                _ ->
                    ( Nothing, Nothing )
    in
        ( currentLocation, currentService, currentConnection )


findById : Int -> WebData (Dict Int a) -> Maybe a
findById id a =
    case a of
        Success a_ ->
            Dict.get id a_

        _ ->
            Nothing


updateGraphAndSim : WebData (Dict Int Service) -> WebData (Dict Int Connection) -> ( Maybe (Graph Entity ()), Maybe (State Int) )
updateGraphAndSim rdServices rdConnections =
    case ( rdServices, rdConnections ) of
        ( Success services, Success connections ) ->
            let
                graph =
                    Pages.Graph.makeGraph (Dict.values services) (Dict.values connections)

                simulation =
                    Pages.Graph.makeSimulation graph
            in
                ( Just graph, Just simulation )

        _ ->
            ( Nothing, Nothing )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        (case model.simulation of
            Just simulation ->
                if Force.isCompleted simulation then
                    Sub.none
                else
                    AnimationFrame.times Tick

            Nothing ->
                Sub.none
        )
            :: [ Navbar.subscriptions model.navbar NavbarMsg ]
