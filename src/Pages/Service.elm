module Pages.Service exposing (Model, Msg, init, update, view)

import Api.Entities exposing (Service)
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
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..))
import Table exposing (defaultCustomizations)


type alias Model =
    { services : WebData (List Service)
    , deleting : Maybe Int
    , deleteAlert : Maybe String
    , tableState : Table.State
    , tableQuery : String
    }


type Msg
    = ResultGetServices (Result Http.Error (List Service))
    | DeleteService Int
    | ResultDeleteService (Result Http.Error String)
    | SetTableQuery String
    | SetTableState Table.State


init : ( Model, Cmd Msg )
init =
    { services = NotAsked
    , deleting = Nothing
    , deleteAlert = Nothing
    , tableState = Table.initialSort "Name"
    , tableQuery = ""
    }
        ! [ Api.getServices ResultGetServices ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultGetServices servicesResult ->
            { model | services = RemoteData.fromResult servicesResult } ! []

        DeleteService id ->
            let
                setNewId =
                    ( { model | deleting = Just id }, Cmd.none )

                ( model_, cmd ) =
                    case model.deleting of
                        Just id_ ->
                            if id_ == id then
                                ( { model | deleting = Nothing }, Api.deleteService id ResultDeleteService )
                            else
                                setNewId

                        Nothing ->
                            setNewId
            in
                model_
                    ! [ cmd ]

        ResultDeleteService (Ok _) ->
            model ! [ Api.getServices ResultGetServices ]

        ResultDeleteService (Err error) ->
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

        lowerQuery =
            String.toLower model.tableQuery

        allProperties =
            \x -> String.join "|" [ x.name, x.hostedOn, x.description ]

        filter =
            List.filter (String.contains lowerQuery << String.toLower << allProperties) <| RemoteData.withDefault [] model.services
    in
        Grid.container []
            [ Grid.row [ Row.attrs [ class "spacer-12" ] ]
                [ Grid.col [ Col.md12 ] [ h5 [] [ text "Services" ] ]
                ]
            , Grid.row []
                [ Grid.col [] [ Button.linkButton [ Button.attrs [ href <| Routing.getLink ServicesAdd ] ] [ text "Add" ] ]
                , Grid.col []
                    [ Input.text
                        [ Input.placeholder "Search"
                        , Input.value model.tableQuery
                        , Input.onInput SetTableQuery
                        ]
                    ]
                ]
            , Grid.row [ Row.attrs [ class "spacer-12" ] ]
                [ Grid.col [ Col.md12 ]
                    [ Table.view (tableConfig model.deleting) model.tableState filter ]
                ]
            ]


tableConfig : Maybe Int -> Table.Config Service Msg
tableConfig deleting =
    let
        tableColumns =
            [ Table.stringColumn "Name" Nothing .name
            , Table.stringColumn "Hosted On" (Just "hidden-xs-down") .hostedOn
            , Table.stringColumn "Description" (Just "hidden-sm-down") .description
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


viewTableButtons : Maybe Int -> Service -> Table.HtmlDetails Msg
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
            [ Button.linkButton [ Button.small, Button.attrs [ href <| Routing.getLink (ServicesConnections id) ] ] [ text "Connections" ]
            , Button.linkButton [ Button.small, Button.attrs [ href <| Routing.getLink (ServicesEdit id) ] ] [ text "Edit" ]
            , Button.linkButton [ Button.small, Button.onClick <| DeleteService id ] [ span [ class deleteClass ] [ text deleteText ] ]
            ]
