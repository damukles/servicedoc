module Pages.Graph exposing (view, makeGraph, makeSimulation, updateGraphWithList)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Html exposing (Html, div)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Svg exposing (Svg, svg, line, text, g, circle, text_)
import Svg.Attributes exposing (r, fill, stroke, strokeWidth, cx, cy, x, y, x1, x2, y1, y2, width, height, class, textAnchor)
import Visualization.Force as Force exposing (State)
import Types exposing (..)


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


nodeElement : { d | id : NodeId, label : { c | value : String, x : Float, y : Float } } -> Svg Msg
nodeElement node =
    g []
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
        , Svg.text_
            [ x (toString node.label.x)
            , y (toString <| node.label.y + 16)
            , textAnchor "middle"
            ]
            [ text node.label.value ]
        ]


view : Model -> Svg Msg
view model =
    let
        svgGraph =
            case model.graph of
                Just graph ->
                    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
                        [ g [ class "links" ] <| List.map (linkElement graph) <| Graph.edges graph
                        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes graph
                        ]

                Nothing ->
                    div [] [ text "loading.." ]
    in
        Grid.container []
            [ Grid.row []
                [ Grid.col []
                    [ Button.button [ Button.onClick RefreshGraph ] [ text "Refresh" ]
                    ]
                ]
            , Grid.row []
                [ Grid.col []
                    [ svgGraph
                    ]
                ]
            ]
