module Pages.Service.Edit exposing (Model, Msg, init, update, view)

import Api.Entities exposing (Service, emptyService)
import Api.Request as Api
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Html exposing (Html, div, text)
import Html.Attributes exposing (for, class)
import Http
import Navigation
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..))
import Validation exposing (isValidIdProp, isValidStringProp)


type alias Model =
    { serviceId : Maybe Int
    , service : WebData Service
    , saveAlert : Maybe String
    }


type Msg
    = ResultGetService (Result Http.Error Service)
    | UpdateService Service
    | SaveService
    | Cancel
    | ResultSaveService (Result Http.Error Service)


init : Maybe Int -> ( Model, Cmd Msg )
init serviceId =
    let
        ( service, serviceCmd ) =
            case serviceId of
                Just id ->
                    ( NotAsked, Api.getService id ResultGetService )

                Nothing ->
                    ( Success emptyService, Cmd.none )
    in
        { serviceId = serviceId
        , service = service
        , saveAlert = Nothing
        }
            ! [ serviceCmd
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultGetService serviceResult ->
            { model | service = RemoteData.fromResult serviceResult } ! []

        UpdateService service ->
            { model | service = Success service } ! []

        SaveService ->
            let
                service =
                    RemoteData.withDefault emptyService model.service

                cmd =
                    case model.serviceId of
                        Just id ->
                            Api.updateService service ResultSaveService

                        Nothing ->
                            Api.addService service ResultSaveService
            in
                model ! [ cmd ]

        Cancel ->
            model ! [ Navigation.newUrl <| Routing.getLink Services ]

        ResultSaveService (Ok service) ->
            { model | service = Success service }
                ! [ Navigation.newUrl <| Routing.getLink Services ]

        ResultSaveService (Err err) ->
            { model | saveAlert = Just (toString err) } ! []


view : Model -> Html Msg
view model =
    let
        alertMessage =
            case model.saveAlert of
                Just message ->
                    Alert.simpleDanger [] [ text message ]

                Nothing ->
                    text ""

        validity isValid =
            if isValid then
                Input.success
            else
                Input.danger
    in
        case model.service of
            Success service ->
                Grid.container []
                    [ Form.form []
                        [ alertMessage
                        , Form.group []
                            [ Form.label [ for "nam" ] [ text "Name" ]
                            , Input.text
                                [ Input.id "nam"
                                , Input.onInput (UpdateService << setName service)
                                , Input.value service.name
                                , validity <| isValidStringProp service.name
                                ]
                            , Form.invalidFeedback [] [ text "Cannot be empty." ]
                            , Form.help [] [ text "The service's name" ]
                            ]
                        , Form.group []
                            [ Form.label [ for "hon" ] [ text "Hosted on" ]
                            , Input.text
                                [ Input.id "hon"
                                , Input.onInput <| (UpdateService << setHostedOn service)
                                , Input.value service.hostedOn
                                , validity <| isValidStringProp service.hostedOn
                                ]
                            , Form.invalidFeedback [] [ text "Cannot be empty." ]
                            , Form.help [] [ text "The server or instance where the service is hosted" ]
                            ]
                        , Form.group []
                            [ Form.label [ for "des" ] [ text "Description" ]
                            , Textarea.textarea
                                [ Textarea.id "des"
                                , Textarea.rows 4
                                , Textarea.onInput <| (UpdateService << setDescription service)
                                , Textarea.value service.description
                                ]
                            , Form.help [] [ text "Whatever is important" ]
                            ]
                        , Button.button [ Button.onClick SaveService, Button.disabled <| not <| isValid service ]
                            [ text "Save" ]
                        , Button.button [ Button.onClick Cancel, Button.attrs [ class "ml-1" ] ] [ text "Cancel" ]
                        ]
                    ]

            Failure service ->
                Grid.container [] [ Alert.simpleDanger [] [ text "Service not found or no connection to server" ] ]

            _ ->
                Grid.container [] [ Alert.simpleDanger [] [ text "loading.." ] ]


setName : { b | name : a } -> c -> { b | name : c }
setName service name =
    { service | name = name }


setHostedOn : { b | hostedOn : a } -> c -> { b | hostedOn : c }
setHostedOn service hostedOn =
    { service | hostedOn = hostedOn }


setDescription : { b | description : a } -> c -> { b | description : c }
setDescription service description =
    { service | description = description }


isValid : { a | hostedOn : String, name : String } -> Bool
isValid { name, hostedOn } =
    isValidStringProp name && isValidStringProp hostedOn
