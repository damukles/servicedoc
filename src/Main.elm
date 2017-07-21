module Main exposing (main)

import App exposing (init, update, subscriptions)
import View exposing (view)
import Types exposing (Model, Msg(UrlChange))
import Navigation exposing (Location)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
