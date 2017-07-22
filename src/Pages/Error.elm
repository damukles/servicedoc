module Pages.Error exposing (view)

import Html exposing (Html, div, p, text)
import Bootstrap.Alert as Alert
import Bootstrap.Grid as Grid


view : String -> Html msg
view message =
    Grid.container []
        [ Alert.danger
            [ text "Error" ]
        , p [] [ text message ]
        ]
