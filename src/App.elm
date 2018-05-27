module App exposing (Model, Msg(..), Page(..), init, update, subscriptions)

import Bootstrap.Navbar as Navbar
import Navigation exposing (Location)
import Pages.Connection
import Pages.Connection.Edit
import Pages.Graph
import Pages.Service
import Pages.Service.Edit
import Routing exposing (Route(..))


type Msg
    = NewUrl String
    | UrlChange (Maybe Route)
    | NavbarMsg Navbar.State
    | ServicesPageMsg Pages.Service.Msg
    | ConnectionsPageMsg Pages.Connection.Msg
    | EditConnectionPageMsg Pages.Connection.Edit.Msg
    | EditServicePageMsg Pages.Service.Edit.Msg
    | GraphPageMsg Pages.Graph.Msg


type alias Model =
    { navbar : Navbar.State
    , page : Page
    }


type Page
    = ServicesPage Pages.Service.Model
    | ConnectionsPage Pages.Connection.Model
    | EditConnectionPage Pages.Connection.Edit.Model
    | EditServicePage Pages.Service.Edit.Model
    | GraphPage Pages.Graph.Model


init : Maybe Route -> ( Model, Cmd Msg )
init route =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        ( page, pageCmd ) =
            pageFromRoute route
    in
        ( { navbar = navbarState
          , page = page
          }
        , Cmd.batch [ navbarCmd, pageCmd ]
        )


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
            connectionsEditPage Nothing

        Just (ConnectionsEdit id) ->
            connectionsEditPage (Just id)

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
            graphPage Nothing

        Just (GraphOf id) ->
            graphPage (Just id)

        Nothing ->
            graphPage Nothing


connectionsEditPage : Maybe Int -> ( Page, Cmd Msg )
connectionsEditPage id =
    let
        ( pageModel, pageCmd ) =
            Pages.Connection.Edit.init id
    in
        ( EditConnectionPage pageModel, Cmd.map EditConnectionPageMsg pageCmd )


graphPage : Maybe Int -> ( Page, Cmd Msg )
graphPage id =
    let
        ( pageModel, pageCmd ) =
            Pages.Graph.init id
    in
        ( GraphPage pageModel, Cmd.map GraphPageMsg pageCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange route ->
            let
                ( page, pageCmd ) =
                    pageFromRoute route
            in
                ( { model | page = page }, pageCmd )

        NavbarMsg state ->
            { model | navbar = state } ! []

        ServicesPageMsg pageMsg ->
            case model.page of
                ServicesPage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Service.update pageMsg pageModel
                    in
                        { model | page = ServicesPage pageModel_ }
                            ! [ Cmd.map ServicesPageMsg pageCmd ]

                _ ->
                    model ! []

        ConnectionsPageMsg pageMsg ->
            case model.page of
                ConnectionsPage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Connection.update pageMsg pageModel
                    in
                        { model | page = ConnectionsPage pageModel_ }
                            ! [ Cmd.map ConnectionsPageMsg pageCmd ]

                _ ->
                    model ! []

        EditConnectionPageMsg pageMsg ->
            case model.page of
                EditConnectionPage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Connection.Edit.update pageMsg pageModel
                    in
                        { model | page = EditConnectionPage pageModel_ }
                            ! [ Cmd.map EditConnectionPageMsg pageCmd ]

                _ ->
                    model ! []

        EditServicePageMsg pageMsg ->
            case model.page of
                EditServicePage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Service.Edit.update pageMsg pageModel
                    in
                        { model | page = EditServicePage pageModel_ }
                            ! [ Cmd.map EditServicePageMsg pageCmd ]

                _ ->
                    model ! []

        GraphPageMsg pageMsg ->
            case model.page of
                GraphPage pageModel ->
                    let
                        ( pageModel_, pageCmd ) =
                            Pages.Graph.update pageMsg pageModel
                    in
                        { model | page = GraphPage pageModel_ }
                            ! [ Cmd.map GraphPageMsg pageCmd ]

                _ ->
                    model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        graphSub =
            case model.page of
                GraphPage subModel ->
                    Sub.map GraphPageMsg <| Pages.Graph.subscriptions subModel

                _ ->
                    Sub.none
    in
        Sub.batch
            [ Navbar.subscriptions model.navbar NavbarMsg
            , graphSub
            ]
