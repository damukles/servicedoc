module View exposing (view)

import Api.Entities exposing (Connection)
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href)
import Pages.Connection
import Pages.Connection.Edit
import Pages.Error
import Pages.Graph
import Pages.Service
import Pages.Service.Edit
import RemoteData exposing (RemoteData(..))
import Routing exposing (Route(..))
import Types exposing (..)
import Util exposing (emptyConnection, emptyService, findById)


view : Model -> Html Msg
view model =
    div []
        [ Grid.container [] [ navbar model ]
        , Grid.container [ class "spacer-12" ] [ content model ]
        ]


navbar : Model -> Html Msg
navbar model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ href "#" ] [ text "Service Doc" ]
        |> Navbar.items
            [ Navbar.itemLink [ href <| Routing.getLink Services ] [ text "Services" ]
            , Navbar.itemLink [ href <| Routing.getLink Connections ] [ text "Connections" ]
            , Navbar.itemLink [ href <| Routing.getLink Graph ] [ text "Graph" ]
            ]
        |> Navbar.view model.navbar


content : Model -> Html Msg
content model =
    case model.currentLocation of
        Just Services ->
            let
                services =
                    valuesFromRemoteDataDict model.services

                config =
                    { stateMsg = ServicesViewMsg }
            in
                Pages.Service.view config model.servicesViewState services

        Just ServicesAdd ->
            case model.currentService of
                Ready service ->
                    let
                        config =
                            { updateMsg = UpdateCurrentService
                            , saveMsg = AddService
                            , alert = model.lastAlert
                            }
                    in
                        Pages.Service.Edit.view config service

                _ ->
                    Pages.Error.view "You found a bug. Please report."

        Just (ServicesEdit id) ->
            case model.currentService of
                NotReady int ->
                    serviceEditView model <| Maybe.withDefault emptyService <| findById id model.services

                Ready service ->
                    serviceEditView model service

                NotFound ->
                    Pages.Error.view "Service not found."

                NoIntention ->
                    Pages.Error.view "You found a bug. Please report."

        Just (ServicesConnections id) ->
            connectionsView (\c -> c.from == id || c.to == id) model

        Just Connections ->
            connectionsView (\_ -> True) model

        Just ConnectionsAdd ->
            case model.currentConnection of
                Ready connection ->
                    let
                        config =
                            { updateMsg = UpdateCurrentConnection
                            , saveMsg = AddConnection
                            , alert = model.lastAlert
                            }

                        services =
                            valuesFromRemoteDataDict model.services
                    in
                        Pages.Connection.Edit.view config services connection

                _ ->
                    Pages.Error.view "You found a bug. Please report."

        Just (ConnectionsEdit id) ->
            case model.currentConnection of
                NotReady id ->
                    connectionEditView model <| Maybe.withDefault emptyConnection <| findById id model.connections

                Ready connection ->
                    connectionEditView model connection

                NotFound ->
                    Pages.Error.view "Connection not found."

                NoIntention ->
                    Pages.Error.view "You found a bug. Please report."

        Just Graph ->
            Pages.Graph.view model

        Nothing ->
            Pages.Graph.view model


serviceEditView : { a | lastAlert : Maybe String } -> Api.Entities.Service -> Html Msg
serviceEditView model service =
    let
        config =
            { updateMsg = UpdateCurrentService
            , saveMsg = EditService
            , alert = model.lastAlert
            }
    in
        Pages.Service.Edit.view config service


connectionEditView :
    { a | lastAlert : Maybe String, services : RemoteData e (Dict.Dict comparable Api.Entities.Service) }
    -> Connection
    -> Html Msg
connectionEditView model connection =
    let
        config =
            { updateMsg = UpdateCurrentConnection
            , saveMsg = EditConnection
            , alert = model.lastAlert
            }

        services =
            valuesFromRemoteDataDict model.services
    in
        Pages.Connection.Edit.view config services connection


connectionsView : (Connection -> Bool) -> Model -> Html Msg
connectionsView filterFunc model =
    let
        services =
            valuesFromRemoteDataDict model.services

        connections =
            valuesFromRemoteDataDict model.connections
                |> List.filter filterFunc

        config =
            { stateMsg = ConnectionsViewMsg }
    in
        Pages.Connection.view config model.connectionsViewState services connections


valuesFromRemoteDataDict : RemoteData e (Dict.Dict comparable v) -> List v
valuesFromRemoteDataDict dict =
    RemoteData.withDefault Dict.empty dict
        |> Dict.values
