module Pages.Service exposing (view, init, State, Config)

import Api.Entities exposing (Service)
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html exposing (Html, div, text)
import Html.Attributes exposing (href, class)
import Routing exposing (Route(..))
import Table exposing (defaultCustomizations)


type alias State =
    { tableState : Table.State
    , tableQuery : String
    }


type alias Config msg =
    { stateMsg : State -> msg
    }


init : State
init =
    { tableState = Table.initialSort "Name"
    , tableQuery = ""
    }


view : Config msg -> State -> List Service -> Html msg
view { stateMsg } state services =
    let
        lowerQuery =
            String.toLower state.tableQuery

        allProperties =
            \x -> String.join "|" [ x.name, x.hostedOn, x.description ]

        filter =
            List.filter (String.contains lowerQuery << String.toLower << allProperties) services
    in
        Grid.container []
            [ Grid.row []
                [ Grid.col [] [ Button.linkButton [ Button.attrs [ href <| Routing.getLink ServicesAdd ] ] [ text "Add" ] ]
                , Grid.col []
                    [ Input.text [ Input.placeholder "Search", Input.onInput <| setTableQuery stateMsg state ] ]
                ]
            , Grid.row [ Row.attrs [ class "spacer-12" ] ]
                [ Grid.col [ Col.md12 ]
                    [ Table.view (tableConfig stateMsg state) state.tableState filter ]
                ]
            ]


setTableQuery : (State -> msg) -> State -> String -> msg
setTableQuery msg state query =
    msg { state | tableQuery = query }


tableConfig : (State -> msg) -> State -> Table.Config Service msg
tableConfig msg state =
    Table.customConfig
        { toId = .name
        , toMsg = setTableState msg state
        , columns =
            [ Table.stringColumn "Name" .name
            , Table.stringColumn "Hosted On" .hostedOn
            , Table.stringColumn "Description" .description
            , Table.veryCustomColumn { name = "Actions", viewData = viewTableButtons, sorter = Table.unsortable }
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "table" ] }
        }


viewTableButtons : Service -> Table.HtmlDetails msg
viewTableButtons { id } =
    Table.HtmlDetails []
        [ Button.linkButton [ Button.small, Button.attrs [ href <| Routing.getLink (ServicesConnections id) ] ] [ text "Connections" ]
        , Button.linkButton [ Button.small, Button.attrs [ href <| Routing.getLink (ServicesEdit id) ] ] [ text "Edit" ]
        ]


setTableState : (State -> msg) -> State -> Table.State -> msg
setTableState msg state tableState =
    msg { state | tableState = tableState }
