module Pages.Connection.Edit exposing (Model, Msg, init, update, view)

import Api.Entities exposing (Connection, Service)
import Api.Request as Api
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Html exposing (Html, div, text)
import Html.Attributes exposing (disabled, for, selected, value)
import Http
import Navigation
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..))
import Util exposing (emptyConnection)
import Validation exposing (isValidIdProp, isValidStringProp)


type alias Model =
    { connectionId : Maybe Int
    , connection : WebData Connection
    , services : WebData (List Service)
    , saveAlert : Maybe String
    }


type Msg
    = ResultGetServices (Result Http.Error (List Service))
    | ResultGetConnection (Result Http.Error Connection)
    | UpdateConnection Connection
    | SaveConnection
    | ResultSaveConnection (Result Http.Error Connection)


init : Maybe Int -> ( Model, Cmd Msg )
init connectionId =
    let
        ( connection, connectionCmd ) =
            case connectionId of
                Just id ->
                    ( NotAsked, Api.getConnection id ResultGetConnection )

                Nothing ->
                    ( Success emptyConnection, Cmd.none )
    in
        { connectionId = connectionId
        , connection = connection
        , services = NotAsked
        , saveAlert = Nothing
        }
            ! [ Api.getServices ResultGetServices
              , connectionCmd
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultGetServices servicesResult ->
            { model | services = RemoteData.fromResult servicesResult } ! []

        ResultGetConnection connectionResult ->
            { model | connection = RemoteData.fromResult connectionResult } ! []

        UpdateConnection connection ->
            { model | connection = Success connection } ! []

        SaveConnection ->
            let
                connection =
                    RemoteData.withDefault emptyConnection model.connection

                cmd =
                    case model.connectionId of
                        Just id ->
                            Api.updateConnection connection ResultSaveConnection

                        Nothing ->
                            Api.addConnection connection ResultSaveConnection
            in
                model ! [ cmd ]

        ResultSaveConnection (Ok connection) ->
            { model | connection = Success connection }
                ! [ Navigation.newUrl <| Routing.getLink Connections ]

        ResultSaveConnection (Err err) ->
            { model | saveAlert = Just (toString err) } ! []


view : Model -> Html Msg
view model =
    let
        alertMessage =
            case model.saveAlert of
                Just message ->
                    Alert.danger [ text message ]

                Nothing ->
                    text ""

        groupDangerIfNot bool =
            if bool then
                []
            else
                [ Form.groupDanger ]
    in
        case ( model.services, model.connection ) of
            ( Success services, Success connection ) ->
                Grid.container []
                    [ Form.form []
                        [ alertMessage
                        , Form.group (groupDangerIfNot <| isValidStringProp connection.name)
                            [ Form.label [ for "nam" ] [ text "Name" ]
                            , Input.text
                                [ Input.id "nam"
                                , Input.onInput (UpdateConnection << setName connection)
                                , Input.value connection.name
                                ]
                            , Form.help [] [ text "The connection's name" ]
                            ]
                        , Form.group (groupDangerIfNot <| isValidIdProp connection.from)
                            [ Form.label [ for "from" ] [ text "From" ]
                            , Select.select
                                [ Select.id "from"
                                , Select.onChange (UpdateConnection << setFrom connection)
                                ]
                              <|
                                serviceSelectOptions connection.from services
                            , Form.help [] [ text "First Endpoint" ]
                            ]
                        , Form.group (groupDangerIfNot <| isValidIdProp connection.to)
                            [ Form.label [ for "to" ] [ text "To" ]
                            , Select.select
                                [ Select.id "to"
                                , Select.onChange (UpdateConnection << setTo connection)
                                ]
                              <|
                                serviceSelectOptions connection.to services
                            , Form.help [] [ text "Second Endpoint" ]
                            ]
                        , Form.group (groupDangerIfNot <| isValidStringProp connection.connectionType)
                            [ Form.label [ for "cont" ] [ text "Connection Type" ]
                            , Input.text
                                [ Input.id "cont"
                                , Input.onInput (UpdateConnection << setConnectionType connection)
                                , Input.value connection.connectionType
                                ]
                            , Form.help [] [ text "The protocol, etc." ]
                            ]
                        , Form.group []
                            [ Form.label [ for "cond" ] [ text "Connection Details" ]
                            , Input.text
                                [ Input.id "cond"
                                , Input.onInput (UpdateConnection << setConnectionDetails connection)
                                , Input.value connection.connectionDetails
                                ]
                            , Form.help [] [ text "Any important details" ]
                            ]
                        , Form.group []
                            [ Form.label [ for "auth" ] [ text "Authentication" ]
                            , Input.text
                                [ Input.id "auth"
                                , Input.onInput (UpdateConnection << setAuthentication connection)
                                , Input.value connection.authentication
                                ]
                            , Form.help [] [ text "The userrepository and username" ]
                            ]
                        , Form.group []
                            [ Form.label [ for "des" ] [ text "Description" ]
                            , Textarea.textarea
                                [ Textarea.id "des"
                                , Textarea.rows 4
                                , Textarea.onInput (UpdateConnection << setDescription connection)
                                , Textarea.value connection.description
                                ]
                            , Form.help [] [ text "Whatever is important" ]
                            ]
                        , Button.button [ Button.onClick SaveConnection, Button.disabled <| not <| isValid connection ]
                            [ text "Save" ]
                        ]
                    ]

            ( Failure services, Failure connection ) ->
                Grid.container [] [ Alert.danger [ text "No connection to server" ] ]

            ( _, Failure connection ) ->
                Grid.container [] [ Alert.danger [ text "Connection not found" ] ]

            ( _, _ ) ->
                Grid.container [] [ Alert.info [ text "loading.." ] ]


serviceSelectOptions : Int -> List Service -> List (Select.Item msg)
serviceSelectOptions id services =
    List.map (viewServiceSelect id) services
        |> addDefaultOption id


viewServiceSelect : Int -> Service -> Select.Item msg
viewServiceSelect id service =
    Select.item [ value <| toString service.id, selected (id == service.id) ] [ text service.name ]


addDefaultOption : Int -> List (Select.Item msg) -> List (Select.Item msg)
addDefaultOption id list =
    if id < 0 then
        (Select.item [ value "-1", selected True, disabled True ] [ text "--- select a service ---" ]) :: list
    else
        list


setName : { b | name : a } -> c -> { b | name : c }
setName connection name =
    { connection | name = name }


setFrom : { b | from : a } -> String -> { b | from : Int }
setFrom connection from =
    { connection | from = Result.withDefault -1 <| String.toInt from }


setTo : { b | to : a } -> String -> { b | to : Int }
setTo connection to =
    { connection | to = Result.withDefault -1 <| String.toInt to }


setConnectionType : { b | connectionType : a } -> c -> { b | connectionType : c }
setConnectionType connection connectionType =
    { connection | connectionType = connectionType }


setConnectionDetails : { b | connectionDetails : a } -> c -> { b | connectionDetails : c }
setConnectionDetails connection connectionDetails =
    { connection | connectionDetails = connectionDetails }


setAuthentication : { b | authentication : a } -> c -> { b | authentication : c }
setAuthentication connection authentication =
    { connection | authentication = authentication }


setDescription : { b | description : a } -> c -> { b | description : c }
setDescription connection description =
    { connection | description = description }


isValid : Connection -> Bool
isValid { name, connectionType, from, to } =
    isValidStringProp name && isValidStringProp connectionType && isValidIdProp from && isValidIdProp to
