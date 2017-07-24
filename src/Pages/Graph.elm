module Pages.Graph exposing (Model, Msg, init, update, subscriptions, view)

import AnimationFrame
import Api.Entities exposing (Connection, Service)
import Api.Request as Api
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Dict exposing (Dict)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html, div)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Svg exposing (Svg, circle, g, line, svg, text, text_)
import Svg.Attributes exposing (class, cx, cy, fill, height, r, stroke, strokeWidth, textAnchor, width, x, x1, x2, y, y1, y2)
import Time exposing (Time)
import Visualization.Force as Force exposing (State)


type alias Model =
    { graph : Maybe (Graph Entity ())
    , simulation : Maybe (Force.State NodeId)
    , showLabels : Bool
    , connections : WebData (Dict Int Connection)
    , services : WebData (Dict Int Service)
    }


type Msg
    = Tick Time
    | ToggleLabels
    | ResultGetConnections (Result Http.Error (List Connection))
    | ResultGetServices (Result Http.Error (List Service))


type alias Entity =
    Force.Entity NodeId { value : String }


init : ( Model, Cmd Msg )
init =
    { graph = Nothing
    , simulation = Nothing
    , showLabels = True
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


updateGraphAndSim : WebData (Dict Int Service) -> WebData (Dict Int Connection) -> ( Maybe (Graph Entity ()), Maybe (State Int) )
updateGraphAndSim rdServices rdConnections =
    case ( rdServices, rdConnections ) of
        ( Success services, Success connections ) ->
            let
                graph =
                    makeGraph (Dict.values services) (Dict.values connections)

                simulation =
                    makeSimulation graph
            in
                ( Just graph, Just simulation )

        _ ->
            ( Nothing, Nothing )


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


makeSimulation : Graph n e -> State Int
makeSimulation graph =
    let
        link { from, to } =
            ( from, to )
    in
        Force.simulation
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]


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
            [ strokeWidth "1"
            , stroke "#aaa"
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
                , fill "#000"
                , stroke "transparent"
                , strokeWidth "7px"
                , cx (toString node.label.x)
                , cy (toString node.label.y)
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
                    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
                        [ g [ class "links" ] <| List.map (linkElement graph) <| Graph.edges graph
                        , g [ class "nodes" ] <| List.map (nodeElement model.showLabels) <| Graph.nodes graph
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
                    ]
                ]
            , Grid.row []
                [ Grid.col []
                    [ svgGraph
                    ]
                ]
            ]
