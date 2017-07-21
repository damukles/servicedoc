module Pages.Connection.Edit exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (for, value, selected)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Form.Select as Select
import Api.Entities exposing (Connection, Service)


type alias Config msg =
    { updateMsg : Connection -> msg
    , saveMsg : Connection -> msg
    , alert : Maybe String
    }


view : Config msg -> List Service -> Connection -> Html msg
view { updateMsg, saveMsg, alert } services connection =
    let
        alertMessage =
            case alert of
                Just message ->
                    Alert.danger [ text message ]

                Nothing ->
                    text ""
    in
        Grid.container []
            [ Form.form []
                [ alertMessage
                , Form.group []
                    [ Form.label [ for "nam" ] [ text "Name" ]
                    , Input.text
                        [ Input.id "nam"
                        , Input.onInput <| setName updateMsg connection
                        , Input.value connection.name
                        ]
                    , Form.help [] [ text "The connection's name" ]
                    ]
                , Form.group []
                    [ Form.label [ for "from" ] [ text "From" ]
                    , Select.select
                        [ Select.id "from"
                        , Select.onChange <| setFrom updateMsg connection
                        ]
                      <|
                        List.map (viewServiceSelect connection.from) services
                    , Form.help [] [ text "First Endpoint" ]
                    ]
                , Form.group []
                    [ Form.label [ for "to" ] [ text "To" ]
                    , Select.select
                        [ Select.id "to"
                        , Select.onChange <| setTo updateMsg connection
                        ]
                      <|
                        List.map (viewServiceSelect connection.to) services
                    , Form.help [] [ text "Second Endpoint" ]
                    ]
                , Form.group []
                    [ Form.label [ for "cont" ] [ text "Connection Type" ]
                    , Input.text
                        [ Input.id "cont"
                        , Input.onInput <| setConnectionType updateMsg connection
                        , Input.value connection.connectionType
                        ]
                    , Form.help [] [ text "The protocol, etc." ]
                    ]
                , Form.group []
                    [ Form.label [ for "cond" ] [ text "Connection Details" ]
                    , Input.text
                        [ Input.id "cond"
                        , Input.onInput <| setConnectionDetails updateMsg connection
                        , Input.value connection.connectionDetails
                        ]
                    , Form.help [] [ text "Any important details" ]
                    ]
                , Form.group []
                    [ Form.label [ for "auth" ] [ text "Authentication" ]
                    , Input.text
                        [ Input.id "auth"
                        , Input.onInput <| setAuthentication updateMsg connection
                        , Input.value connection.authentication
                        ]
                    , Form.help [] [ text "The userrepository and username" ]
                    ]
                , Form.group []
                    [ Form.label [ for "des" ] [ text "Description" ]
                    , Textarea.textarea
                        [ Textarea.id "des"
                        , Textarea.rows 4
                        , Textarea.onInput <| setDescription updateMsg connection
                        , Textarea.value connection.description
                        ]
                    , Form.help [] [ text "Whatever is important" ]
                    ]
                , Button.button [ Button.onClick <| saveMsg connection, Button.disabled <| isInvalid connection ]
                    [ text "Save" ]
                ]
            ]


viewServiceSelect : Int -> Service -> Select.Item msg
viewServiceSelect id service =
    Select.item [ value <| toString service.id, selected (id == service.id) ] [ text service.name ]


setName : ({ b | name : a } -> msg) -> { b | name : d } -> a -> msg
setName msg connection name =
    msg { connection | name = name }


setFrom : ({ a | from : Int } -> b) -> { a | from : c } -> String -> b
setFrom msg connection from =
    msg { connection | from = Result.withDefault -1 <| String.toInt from }


setTo : ({ a | to : Int } -> b) -> { a | to : c } -> String -> b
setTo msg connection to =
    msg { connection | to = Result.withDefault -1 <| String.toInt to }


setConnectionType : ({ b | connectionType : a } -> msg) -> { b | connectionType : d } -> a -> msg
setConnectionType msg connection connectionType =
    msg { connection | connectionType = connectionType }


setConnectionDetails : ({ b | connectionDetails : a } -> msg) -> { b | connectionDetails : d } -> a -> msg
setConnectionDetails msg connection connectionDetails =
    msg { connection | connectionDetails = connectionDetails }


setAuthentication : ({ b | authentication : a } -> msg) -> { b | authentication : d } -> a -> msg
setAuthentication msg connection authentication =
    msg { connection | authentication = authentication }


setDescription : ({ b | description : a } -> msg) -> { b | description : d } -> a -> msg
setDescription msg connection description =
    msg { connection | description = description }


isInvalid :
    { a
        | authentication : String
        , connectionType : String
        , from : Int
        , name : String
        , to : Int
    }
    -> Bool
isInvalid { name, connectionType, authentication, from, to } =
    String.length name < 2 || String.length connectionType < 2 || String.length authentication < 2 || from < 0 || to < 0
