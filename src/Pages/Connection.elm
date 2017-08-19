module Pages.Connection exposing (Model, Msg, init, update, view)

import Api.Entities exposing (Connection, Service)
import Api.Request as Api
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html exposing (Html, div, text, h5, span)
import Html.Attributes exposing (class, href)
import Http
import RemoteData exposing (WebData, RemoteData(..))
import Routing exposing (Route(..))
import Table exposing (defaultCustomizations)


type alias Model =
    { connections : WebData (List Connection)
    , services : WebData (List Service)
    , serviceId : Maybe Int
    , deleting : Maybe Int
    , deleteAlert : Maybe String
    , tableState : Table.State
    , tableQuery : String
    }


type Msg
    = ResultGetConnections (Result Http.Error (List Connection))
    | ResultGetServices (Result Http.Error (List Service))
    | DeleteConnection Int
    | ResultDeleteConnection (Result Http.Error String)
    | SetTableQuery String
    | SetTableState Table.State


init : Maybe Int -> ( Model, Cmd Msg )
init id =
    { connections = NotAsked
    , services = NotAsked
    , serviceId = id
    , deleting = Nothing
    , deleteAlert = Nothing
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

        DeleteConnection id ->
            let
                setNewId =
                    ( { model | deleting = Just id }, Cmd.none )

                ( model_, cmd ) =
                    case model.deleting of
                        Just id_ ->
                            if id_ == id then
                                ( { model | deleting = Nothing }, Api.deleteConnection id_ ResultDeleteConnection )
                            else
                                setNewId

                        Nothing ->
                            setNewId
            in
                model_ ! [ cmd ]

        ResultDeleteConnection (Ok _) ->
            model ! [ Api.getConnections ResultGetConnections ]

        ResultDeleteConnection (Err error) ->
            { model | deleteAlert = Just (toString error) } ! []

        SetTableQuery query ->
            { model | tableQuery = query } ! []

        SetTableState state ->
            { model | tableState = state } ! []


view : Model -> Html Msg
view model =
    let
        alertMessage =
            case model.deleteAlert of
                Just message ->
                    Alert.danger [ text message ]

                Nothing ->
                    text ""

        connections =
            RemoteData.withDefault [] model.connections

        ( connections_, title ) =
            case model.serviceId of
                Just id ->
                    ( List.filter (\c -> c.from == id || c.to == id) connections
                    , (List.filter ((==) id << .id) (RemoteData.withDefault [] model.services)
                        |> List.head
                        |> Maybe.map .name
                        |> Maybe.withDefault ""
                      )
                        ++ " Connections"
                    )

                Nothing ->
                    ( connections, "Connections" )

        lowerQuery =
            String.toLower model.tableQuery

        allProperties =
            \x -> String.join "|" [ x.name, x.connectionType, x.connectionDetails, x.authentication, x.description ]

        filter =
            List.filter (String.contains lowerQuery << String.toLower << allProperties) connections_
    in
        Grid.container []
            [ Grid.row [ Row.attrs [ class "spacer-12" ] ]
                [ Grid.col [ Col.md12 ] [ h5 [] [ text title ] ]
                ]
            , Grid.row []
                [ Grid.col [ Col.md4 ]
                    [ Button.linkButton
                        [ Button.attrs [ href <| Routing.getLink ConnectionsAdd ] ]
                        [ text "Add" ]
                    ]
                , Grid.col [ Col.md8 ]
                    [ Input.text
                        [ Input.placeholder "Search"
                        , Input.value model.tableQuery
                        , Input.onInput SetTableQuery
                        ]
                    ]
                ]
            , Grid.row [ Row.attrs [ class "spacer-12" ] ]
                [ Grid.col [ Col.md12 ]
                    [ Table.view (tableConfig model.deleting <| RemoteData.withDefault [] model.services) model.tableState filter ]
                ]
            ]


tableConfig : Maybe Int -> List Service -> Table.Config Connection Msg
tableConfig deleting services =
    let
        tableColumns =
            [ Table.stringColumn "Name" Nothing .name
            , Table.stringColumn "From" (Just "hidden-sm-down") ((toName services) << .from)
            , Table.stringColumn "To" (Just "hidden-sm-down") ((toName services) << .to)
            , Table.stringColumn "Connection Type" (Just "hidden-md-down") .connectionType
            , Table.stringColumn "Authentication" (Just "hidden-md-down") .authentication
            , Table.veryCustomColumn
                { name = "Actions"
                , viewData = viewTableButtons deleting
                , sorter = Table.unsortable
                , colClass = Nothing
                }
            ]
    in
        Table.customConfig
            { toId = .name
            , toMsg = SetTableState
            , columns = tableColumns
            , customizations = { defaultCustomizations | tableAttrs = [ class "table" ] }
            }


toName : List { b | id : a, name : String } -> a -> String
toName services id =
    List.filter (\x -> x.id == id) services
        |> List.head
        |> Maybe.map .name
        |> Maybe.withDefault "Error"


viewTableButtons : Maybe Int -> Connection -> Table.HtmlDetails Msg
viewTableButtons deleting { id } =
    let
        usualMode =
            ( "Delete", "" )

        ( deleteText, deleteClass ) =
            case deleting of
                Just deletingId ->
                    if deletingId == id then
                        ( "Sure?", "delete-sure" )
                    else
                        usualMode

                Nothing ->
                    usualMode
    in
        Table.HtmlDetails []
            [ Button.linkButton [ Button.small, Button.attrs [ href <| Routing.getLink (ConnectionsEdit id) ] ] [ text "Edit" ]
            , Button.linkButton [ Button.small, Button.onClick <| DeleteConnection id ] [ span [ class deleteClass ] [ text deleteText ] ]
            ]
