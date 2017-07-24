module Pages.Service exposing (Model, Msg, init, update, view)

import Api.Entities exposing (Service)
import Api.Request as Api
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..))
import Table exposing (defaultCustomizations)


type alias Model =
    { services : WebData (List Service)
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
            model ! [ Api.deleteService id ResultDeleteService ]

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
            [ Grid.row []
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
                    [ Table.view tableConfig model.tableState filter ]
                ]
            ]


tableConfig : Table.Config Service Msg
tableConfig =
    Table.customConfig
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Name" .name
            , Table.stringColumn "Hosted On" .hostedOn
            , Table.stringColumn "Description" .description
            , Table.veryCustomColumn { name = "Actions", viewData = viewTableButtons, sorter = Table.unsortable }
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "table" ] }
        }


viewTableButtons : Service -> Table.HtmlDetails Msg
viewTableButtons { id } =
    Table.HtmlDetails []
        [ Button.linkButton [ Button.small, Button.attrs [ href <| Routing.getLink (ServicesConnections id) ] ] [ text "Connections" ]
        , Button.linkButton [ Button.small, Button.attrs [ href <| Routing.getLink (ServicesEdit id) ] ] [ text "Edit" ]
        , Button.linkButton [ Button.small, Button.onClick <| DeleteService id ] [ text "Delete" ]
        ]
