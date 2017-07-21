module Pages.Error exposing (view)

import Html exposing (Html, div, text)


view : String -> Html msg
view message =
    div [] [ text message ]
