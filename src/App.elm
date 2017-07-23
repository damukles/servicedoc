module App exposing (init, update, subscriptions)

import AnimationFrame
import Api.Entities exposing (Connection, Service)
import Api.Request as Api
import Bootstrap.Navbar as Navbar
import Dict exposing (Dict)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Navigation exposing (Location)
import Pages.Connection
import Pages.Connection.Edit
import Pages.Graph
import Pages.Service
import Pages.Service.Edit
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..))
import Types exposing (..)
import Visualization.Force as Force exposing (State)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        currentLocation =
            Routing.parse location

        ( page, cmd ) =
            pageFromRoute currentLocation
    in
        { currentLocation = currentLocation
        , history = []
        , navbar = navbarState
        , servicesViewState = Pages.Service.init
        , connectionsViewState = Pages.Connection.init
        , services = NotAsked
        , connections = NotAsked
        , subPage = page
        , graph = Nothing
        , simulation = Nothing
        }
            ! [ navbarCmd
              , Api.getServices ResultGetServices
              , Api.getConnections ResultGetConnections
              , cmd
              ]


pageFromRoute : Maybe Route -> ( Page, Cmd Msg )
pageFromRoute route =
    case route of
        Just ConnectionsAdd ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Connection.Edit.init Nothing
            in
                ( EditConnectionPage pageModel, Cmd.map EditConnectionPageMsg pageCmd )

        Just (ConnectionsEdit id) ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Connection.Edit.init (Just id)
            in
                ( EditConnectionPage pageModel, Cmd.map EditConnectionPageMsg pageCmd )

        Just ServicesAdd ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Service.Edit.init Nothing
            in
                ( EditServicePage pageModel, Cmd.map EditServicePageMsg pageCmd )

        Just (ServicesEdit id) ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Service.Edit.init (Just id)
            in
                ( EditServicePage pageModel, Cmd.map EditServicePageMsg pageCmd )

        _ ->
            ( None, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ graph, simulation } as model) =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            let
                currentLocation =
                    Routing.parse location

                ( page, cmd ) =
                    pageFromRoute currentLocation
            in
                { model
                    | history = location :: model.history
                    , currentLocation = currentLocation
                    , subPage = page
                }
                    ! [ cmd ]

        NavbarMsg state ->
            { model | navbar = state } ! []

        EditConnectionPageMsg pageMsg ->
            case model.subPage of
                EditConnectionPage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Connection.Edit.update pageMsg pageModel
                    in
                        { model | subPage = EditConnectionPage pageModel_ }
                            ! [ Cmd.map EditConnectionPageMsg pageCmd ]

                _ ->
                    model ! []

        EditServicePageMsg pageMsg ->
            case model.subPage of
                EditServicePage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Service.Edit.update pageMsg pageModel
                    in
                        { model | subPage = EditServicePage pageModel_ }
                            ! [ Cmd.map EditServicePageMsg pageCmd ]

                _ ->
                    model ! []

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

        -- AddService service ->
        --     model ! [ Api.addService service ResultAddService ]
        --
        -- ResultAddService (Ok service) ->
        --     let
        --         ( services_, cmd ) =
        --             case model.services of
        --                 Success data ->
        --                     ( Success <| Dict.insert service.id service data, Cmd.none )
        --
        --                 _ ->
        --                     ( model.services, Api.getServices ResultGetServices )
        --     in
        --         { model | services = services_, lastAlert = Nothing } ! [ cmd, Navigation.back 1 ]
        --
        -- ResultAddService (Err err) ->
        --     let
        --         out =
        --             Debug.log "ResultAddService Error" <| toString err
        --     in
        --         { model | lastAlert = Just Util.oops } ! []
        --
        -- UpdateCurrentService service ->
        --     { model | currentService = Ready service } ! []
        --
        -- EditService service ->
        --     model ! [ Api.updateService service ResultUpdateService ]
        --
        -- ResultUpdateService (Ok service) ->
        --     let
        --         services_ =
        --             RemoteData.map (Dict.insert service.id service) model.services
        --     in
        --         { model | services = services_, lastAlert = Nothing } ! [ Navigation.back 1 ]
        --
        -- ResultUpdateService (Err err) ->
        --     let
        --         out =
        --             Debug.log "ResultUpdateService Error" <| toString err
        --     in
        --         { model | lastAlert = Just Util.oops } ! []
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
