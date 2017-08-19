module Pages.Graph exposing (Model, Msg, init, update, subscriptions, view)

import AnimationFrame
import Api.Entities exposing (Connection, Service)
import Api.Request as Api
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html, div)
import Html.Attributes as Html
import Http
import Navigation
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..))
import Svg exposing (Svg, circle, g, line, svg, text, text_)
import Svg.Attributes exposing (class, cx, cy, height, r, viewBox, preserveAspectRatio, textAnchor, width, x, x1, x2, y, y1, y2)
import Svg.Events exposing (onClick)
import Time exposing (Time)
import Visualization.Force as Force exposing (State)


type alias Model =
    { graph : Maybe (Graph Entity ())
    , simulation : Maybe (Force.State NodeId)
    , showLabels : Bool
    , serviceId : Maybe Int
    , connections : WebData (Dict Int Connection)
    , services : WebData (Dict Int Service)
    }


type Msg
    = Tick Time
    | ToggleLabels
    | ShowGraphOf Int
    | ResultGetConnections (Result Http.Error (List Connection))
    | ResultGetServices (Result Http.Error (List Service))


type alias Entity =
    Force.Entity NodeId { value : String }


init : Maybe Int -> ( Model, Cmd Msg )
init serviceId =
    { graph = Nothing
    , simulation = Nothing
    , showLabels = True
    , serviceId = serviceId
    , connections = NotAsked
    , services = NotAsked
    }
        ! [ Api.getConnections ResultGetConnections
          , Api.getServices ResultGetServices
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ graph, simulation } as model) =
    case msg of
        Tick t ->
            case ( graph, simulation ) of
                ( Just realGraph, Just realSimulation ) ->
                    let
                        ( simulation_, list ) =
                            Force.tick realSimulation <| List.map .label <| Graph.nodes realGraph

                        graph_ =
                            (updateGraphWithList realGraph list)
                    in
                        { model | graph = Just graph_, simulation = Just simulation_ } ! []

                _ ->
                    model ! []

        ToggleLabels ->
            { model | showLabels = not model.showLabels } ! []

        ShowGraphOf id ->
            model ! [ Navigation.newUrl <| Routing.getLink (GraphOf id) ]

        ResultGetServices (Ok services) ->
            let
                services_ =
                    Success <| Dict.fromList <| List.map (\x -> ( x.id, x )) services

                ( graph_, simulation_ ) =
                    updateGraphAndSim model.serviceId services_ model.connections
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
                    updateGraphAndSim model.serviceId model.services connections_
            in
                { model
                    | connections = connections_
                    , graph = graph_
                    , simulation = simulation_
                }
                    ! []

        ResultGetConnections (Err err) ->
            { model | connections = Failure err } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.simulation of
        Just simulation ->
            if Force.isCompleted simulation then
                Sub.none
            else
                AnimationFrame.times Tick

        Nothing ->
            Sub.none


updateGraphAndSim : Maybe Int -> WebData (Dict Int Service) -> WebData (Dict Int Connection) -> ( Maybe (Graph Entity ()), Maybe (State Int) )
updateGraphAndSim serviceId rdServices rdConnections =
    case ( rdServices, rdConnections ) of
        ( Success services, Success connections ) ->
            let
                ( services_, connections_, distance ) =
                    case serviceId of
                        Just id ->
                            let
                                serviceConnections =
                                    List.filter (\c -> c.from == id || c.to == id) (Dict.values connections)
                            in
                                ( List.filter (isInToOrFrom serviceConnections) (Dict.values services)
                                , serviceConnections
                                , 160
                                )

                        Nothing ->
                            ( Dict.values services, Dict.values connections, 100 )

                graph =
                    makeGraph services_ connections_

                simulation =
                    makeSimulation distance graph
            in
                ( Just graph, Just simulation )

        _ ->
            ( Nothing, Nothing )


isInToOrFrom : List Connection -> Service -> Bool
isInToOrFrom connections { id } =
    let
        filteredConnections =
            List.filter (\c -> c.from == id || c.to == id) connections
    in
        if List.length filteredConnections > 0 then
            True
        else
            False


screenWidth : Float
screenWidth =
    1000


screenHeight : Float
screenHeight =
    600


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
        { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
        List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


makeGraph : List { b | id : NodeId, name : c } -> List { a | from : NodeId, to : NodeId } -> Graph (Force.Entity Int { value : c }) ()
makeGraph services connections =
    Graph.fromNodesAndEdges (nodes services) (edges connections)
        |> Graph.mapContexts
            (\({ node } as ctx) ->
                { ctx | node = { label = Force.entity node.id node.label, id = node.id } }
            )


makeSimulation : Float -> Graph n e -> State Int
makeSimulation dist graph =
    Force.simulation
        [ Force.customLinks 1 <| List.map (link dist) <| Graph.edges graph
        , Force.manyBody <| List.map .id <| Graph.nodes graph
        , Force.center (screenWidth / 2) (screenHeight / 2)
        ]


link : b -> { e | from : c, to : d } -> { distance : b, source : c, strength : Maybe a, target : d }
link dist { from, to } =
    { source = from, target = to, distance = dist, strength = Nothing }


nodes : List { b | id : Graph.NodeId, name : a } -> List (Graph.Node a)
nodes vms =
    List.map (\x -> Graph.Node x.id x.name) vms


edges : List { a | from : NodeId, to : NodeId } -> List (Edge ())
edges connections =
    List.map (\x -> Graph.Edge x.from x.to ()) connections


linkElement : Graph Entity e -> { a | from : NodeId, to : NodeId } -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
        line
            [ class "graph-link"
            , x1 (toString source.x)
            , y1 (toString source.y)
            , x2 (toString target.x)
            , y2 (toString target.y)
            ]
            []


nodeElement : Bool -> { d | id : NodeId, label : { c | value : String, x : Float, y : Float } } -> Svg Msg
nodeElement showLabels node =
    let
        label =
            if showLabels then
                [ Svg.text_
                    [ x (toString node.label.x)
                    , y (toString <| node.label.y + 16)
                    , textAnchor "middle"
                    ]
                    [ text node.label.value ]
                ]
            else
                []
    in
        g [] <|
            [ circle
                [ r "3.5"
                , class "graph-node"
                , cx (toString node.label.x)
                , cy (toString node.label.y)
                , onClick <| ShowGraphOf node.id
                ]
                [ Svg.title [] [ text node.label.value ]
                ]
            ]
                ++ label


view : Model -> Svg Msg
view model =
    let
        svgGraph =
            case model.graph of
                Just graph ->
                    svg
                        [ viewBox <| "0 0 " ++ (toString screenWidth) ++ " " ++ (toString screenHeight)
                        , preserveAspectRatio "xMidYMid meet"
                        , class "graph-svg"
                        ]
                        [ g [] <| List.map (linkElement graph) <| Graph.edges graph
                        , g [] <| List.map (nodeElement model.showLabels) <| Graph.nodes graph
                        ]

                Nothing ->
                    div [] [ text "loading.." ]

        buttonText =
            if model.showLabels then
                "Hide Labels"
            else
                "Show Labels"
    in
        Grid.container []
            [ Grid.row []
                [ Grid.col []
                    [ Button.button [ Button.onClick ToggleLabels ] [ text buttonText ]
                    , Html.span [ Html.style [ ( "margin-left", "24px" ) ] ] [ text "Click on a node to zoom in.." ]
                    ]
                ]
            , Grid.row []
                [ Grid.col [ Col.md12 ]
                    [ svgGraph
                    ]
                ]
            ]
