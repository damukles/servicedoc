module Main exposing (main)

import App exposing (Model, Msg(UrlChange), init, update, subscriptions)
import View exposing (view)
import Navigation exposing (Location)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
