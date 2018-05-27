module Main exposing (main)

import App exposing (Model, Msg(UrlChange), init, subscriptions, update)
import Navigation exposing (Location)
import Routing
import View exposing (view)


main : Program Never Model Msg
main =
    Navigation.program (Routing.parse >> UrlChange)
        { init = Routing.parse >> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
