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
            case model.subPage of
                EditServicePage pageModel ->
                    Html.map EditServicePageMsg <| Pages.Service.Edit.view pageModel

                _ ->
                    Pages.Error.view "You found a bug. Please report."

        Just (ServicesEdit id) ->
            case model.subPage of
                EditServicePage pageModel ->
                    Html.map EditServicePageMsg <| Pages.Service.Edit.view pageModel

                _ ->
                    Pages.Error.view "You found a bug. Please report."

        Just (ServicesConnections id) ->
            connectionsView (\c -> c.from == id || c.to == id) model

        Just Connections ->
            connectionsView (\_ -> True) model

        Just ConnectionsAdd ->
            case model.subPage of
                EditConnectionPage pageModel ->
                    Html.map EditConnectionPageMsg <| Pages.Connection.Edit.view pageModel

                _ ->
                    Pages.Error.view "You found a bug. Please report."

        Just (ConnectionsEdit id) ->
            case model.subPage of
                EditConnectionPage pageModel ->
                    Html.map EditConnectionPageMsg <| Pages.Connection.Edit.view pageModel

                _ ->
                    Pages.Error.view "You found a bug. Please report."

        Just Graph ->
            Pages.Graph.view model

        Nothing ->
            Pages.Graph.view model


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
