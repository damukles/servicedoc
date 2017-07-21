module Views.Service exposing (view)

import Html exposing (Html, div, text)
import Types exposing (..)
import Api.Entities exposing (Service, Connection)


view : Service -> Html Msg
view service =
    div [] [ text service.name ]
