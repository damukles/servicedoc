module App exposing (init, update, subscriptions)

import Bootstrap.Navbar as Navbar
import Navigation exposing (Location)
import Pages.Connection
import Pages.Connection.Edit
import Pages.Graph
import Pages.Service
import Pages.Service.Edit
import Routing exposing (Route(..))
import Types exposing (..)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        currentLocation =
            Routing.parse location

        ( page, pageCmd ) =
            pageFromRoute currentLocation
    in
        { currentLocation = currentLocation
        , history = []
        , navbar = navbarState
        , subPage = page
        }
            ! [ navbarCmd
              , pageCmd
              ]


pageFromRoute : Maybe Route -> ( Page, Cmd Msg )
pageFromRoute route =
    case route of
        Just Services ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Service.init
            in
                ( ServicesPage pageModel, Cmd.map ServicesPageMsg pageCmd )

        Just Connections ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Connection.init Nothing
            in
                ( ConnectionsPage pageModel, Cmd.map ConnectionsPageMsg pageCmd )

        Just (ServicesConnections id) ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Connection.init (Just id)
            in
                ( ConnectionsPage pageModel, Cmd.map ConnectionsPageMsg pageCmd )

        Just ConnectionsAdd ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Connection.Edit.init Nothing
            in
                ( EditConnectionPage pageModel, Cmd.map EditConnectionPageMsg pageCmd )

        Just (ConnectionsEdit id) ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Connection.Edit.init (Just id)
            in
                ( EditConnectionPage pageModel, Cmd.map EditConnectionPageMsg pageCmd )

        Just ServicesAdd ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Service.Edit.init Nothing
            in
                ( EditServicePage pageModel, Cmd.map EditServicePageMsg pageCmd )

        Just (ServicesEdit id) ->
            let
                ( pageModel, pageCmd ) =
                    Pages.Service.Edit.init (Just id)
            in
                ( EditServicePage pageModel, Cmd.map EditServicePageMsg pageCmd )

        Just Graph ->
            initGraphPage

        Nothing ->
            initGraphPage


initGraphPage : ( Page, Cmd Msg )
initGraphPage =
    let
        ( pageModel, pageCmd ) =
            Pages.Graph.init
    in
        ( GraphPage pageModel, Cmd.map GraphPageMsg pageCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            let
                currentLocation =
                    Routing.parse location

                ( page, cmd ) =
                    pageFromRoute currentLocation
            in
                { model
                    | history = location :: model.history
                    , currentLocation = currentLocation
                    , subPage = page
                }
                    ! [ cmd ]

        NavbarMsg state ->
            { model | navbar = state } ! []

        ServicesPageMsg pageMsg ->
            case model.subPage of
                ServicesPage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Service.update pageMsg pageModel
                    in
                        { model | subPage = ServicesPage pageModel_ }
                            ! [ Cmd.map ServicesPageMsg pageCmd ]

                _ ->
                    model ! []

        ConnectionsPageMsg pageMsg ->
            case model.subPage of
                ConnectionsPage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Connection.update pageMsg pageModel
                    in
                        { model | subPage = ConnectionsPage pageModel_ }
                            ! [ Cmd.map ConnectionsPageMsg pageCmd ]

                _ ->
                    model ! []

        EditConnectionPageMsg pageMsg ->
            case model.subPage of
                EditConnectionPage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Connection.Edit.update pageMsg pageModel
                    in
                        { model | subPage = EditConnectionPage pageModel_ }
                            ! [ Cmd.map EditConnectionPageMsg pageCmd ]

                _ ->
                    model ! []

        EditServicePageMsg pageMsg ->
            case model.subPage of
                EditServicePage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Service.Edit.update pageMsg pageModel
                    in
                        { model | subPage = EditServicePage pageModel_ }
                            ! [ Cmd.map EditServicePageMsg pageCmd ]

                _ ->
                    model ! []

        GraphPageMsg pageMsg ->
            case model.subPage of
                GraphPage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Graph.update pageMsg pageModel
                    in
                        { model | subPage = GraphPage pageModel_ }
                            ! [ Cmd.map GraphPageMsg pageCmd ]

                _ ->
                    model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        graphSub =
            case model.subPage of
                GraphPage subModel ->
                    Sub.map GraphPageMsg <| Pages.Graph.subscriptions subModel

                _ ->
                    Sub.none
    in
        Sub.batch
            [ Navbar.subscriptions model.navbar NavbarMsg
            , graphSub
            ]
