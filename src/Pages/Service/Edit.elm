module Pages.Service.Edit exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (for)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Api.Entities exposing (Service)
import Validation exposing (isValidStringProp, isValidIdProp)


type alias Config msg =
    { updateMsg : Service -> msg
    , saveMsg : Service -> msg
    , alert : Maybe String
    }


view : Config msg -> Service -> Html msg
view { updateMsg, saveMsg, alert } service =
    let
        alertMessage =
            case alert of
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
        Grid.container []
            [ Form.form []
                [ alertMessage
                , Form.group (groupDangerIfNot <| isValidStringProp service.name)
                    [ Form.label [ for "nam" ] [ text "Name" ]
                    , Input.text
                        [ Input.id "nam"
                        , Input.onInput <| setName updateMsg service
                        , Input.value service.name
                        ]
                    , Form.help [] [ text "The service's name" ]
                    ]
                , Form.group (groupDangerIfNot <| isValidStringProp service.hostedOn)
                    [ Form.label [ for "hon" ] [ text "Hosted on" ]
                    , Input.text
                        [ Input.id "hon"
                        , Input.onInput <| setHostedOn updateMsg service
                        , Input.value service.hostedOn
                        ]
                    , Form.help [] [ text "The server or instance where the service is hosted" ]
                    ]
                , Form.group []
                    [ Form.label [ for "des" ] [ text "Description" ]
                    , Textarea.textarea
                        [ Textarea.id "des"
                        , Textarea.rows 4
                        , Textarea.onInput <| setDescription updateMsg service
                        , Textarea.value service.description
                        ]
                    , Form.help [] [ text "Whatever is important" ]
                    ]
                , Button.button [ Button.onClick <| saveMsg service, Button.disabled <| not <| isValid service ]
                    [ text "Save" ]
                ]
            ]


setName : ({ b | name : a } -> msg) -> { b | name : d } -> a -> msg
setName msg service name =
    msg { service | name = name }


setHostedOn : ({ b | hostedOn : a } -> msg) -> { b | hostedOn : d } -> a -> msg
setHostedOn msg service hostedOn =
    msg { service | hostedOn = hostedOn }


setDescription : ({ b | description : a } -> msg) -> { b | description : d } -> a -> msg
setDescription msg service description =
    msg { service | description = description }


isValid : { a | hostedOn : String, name : String } -> Bool
isValid { name, hostedOn } =
    isValidStringProp name && isValidStringProp hostedOn
