module Pages.Connection exposing (Model, Msg, init, update, view)

import Api.Entities exposing (Connection, Service)
import Api.Request as Api
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href)
import Http
import RemoteData exposing (WebData, RemoteData(..))
import Routing exposing (Route(..))
import Table exposing (defaultCustomizations)


type alias Model =
    { connections : WebData (List Connection)
    , services : WebData (List Service)
    , serviceId : Maybe Int
    , tableState : Table.State
    , tableQuery : String
    }


type Msg
    = ResultGetConnections (Result Http.Error (List Connection))
    | ResultGetServices (Result Http.Error (List Service))
    | SetTableQuery String
    | SetTableState Table.State


init : Maybe Int -> ( Model, Cmd Msg )
init id =
    { connections = NotAsked
    , services = NotAsked
    , serviceId = id
    , tableState = Table.initialSort "Name"
    , tableQuery = ""
    }
        ! [ Api.getConnections ResultGetConnections
          , Api.getServices ResultGetServices
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultGetConnections connectionsResult ->
            { model | connections = RemoteData.fromResult connectionsResult } ! []

        ResultGetServices servicesResult ->
            { model | services = RemoteData.fromResult servicesResult } ! []

        SetTableQuery query ->
            { model | tableQuery = query } ! []

        SetTableState state ->
            { model | tableState = state } ! []


view : Model -> Html Msg
view model =
    let
        connections =
            RemoteData.withDefault [] model.connections

        connections_ =
            case model.serviceId of
                Just id ->
                    List.filter (\c -> c.from == id || c.to == id) connections

                Nothing ->
                    connections

        lowerQuery =
            String.toLower model.tableQuery

        allProperties =
            \x -> String.join "|" [ x.name, x.connectionType, x.connectionDetails, x.authentication, x.description ]

        filter =
            List.filter (String.contains lowerQuery << String.toLower << allProperties) connections_
    in
        Grid.container []
            [ Grid.row []
                [ Grid.col [] [ Button.linkButton [ Button.attrs [ href <| Routing.getLink ConnectionsAdd ] ] [ text "Add" ] ]
                , Grid.col []
                    [ Input.text [ Input.placeholder "Search", Input.onInput SetTableQuery ] ]
                ]
            , Grid.row [ Row.attrs [ class "spacer-12" ] ]
                [ Grid.col [ Col.md12 ]
                    [ Table.view (tableConfig <| RemoteData.withDefault [] model.services) model.tableState filter ]
                ]
            ]


tableConfig : List Service -> Table.Config Connection Msg
tableConfig services =
    Table.customConfig
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Name" .name
            , Table.stringColumn "From" ((toName services) << .from)
            , Table.stringColumn "To" ((toName services) << .to)
            , Table.stringColumn "Connection Type" .connectionType
            , Table.stringColumn "Connection Details" .connectionDetails
            , Table.stringColumn "Authentication" .authentication
            , Table.stringColumn "Description" .description
            , Table.veryCustomColumn { name = "Actions", viewData = viewTableButtons, sorter = Table.unsortable }
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "table" ] }
        }


toName : List { b | id : a, name : String } -> a -> String
toName services id =
    List.filter (\x -> x.id == id) services
        |> List.head
        |> Maybe.map .name
        |> Maybe.withDefault "Error"


viewTableButtons : Connection -> Table.HtmlDetails msg
viewTableButtons { id } =
    Table.HtmlDetails []
        [ Button.linkButton [ Button.small, Button.attrs [ href <| Routing.getLink (ConnectionsEdit id) ] ] [ text "Edit" ]
        ]
