module View exposing (view)

import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href)
import Pages.Connection
import Pages.Connection.Edit
import Pages.Graph
import Pages.Service
import Pages.Service.Edit
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
    case model.subPage of
        ServicesPage pageModel ->
            Html.map ServicesPageMsg <| Pages.Service.view pageModel

        ConnectionsPage pageModel ->
            Html.map ConnectionsPageMsg <| Pages.Connection.view pageModel

        EditConnectionPage pageModel ->
            Html.map EditConnectionPageMsg <| Pages.Connection.Edit.view pageModel

        EditServicePage pageModel ->
            Html.map EditServicePageMsg <| Pages.Service.Edit.view pageModel

        GraphPage pageModel ->
            Html.map GraphPageMsg <| Pages.Graph.view pageModel
