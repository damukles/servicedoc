module Util exposing (..)

import Dict exposing (Dict)
import Api.Entities exposing (Connection, Service)
import RemoteData exposing (WebData, RemoteData(..))
import Types exposing (Editable(..))


editableById : Int -> WebData (Dict Int a) -> Editable Int a
editableById id a =
    case a of
        Success a_ ->
            case Dict.get id a_ of
                Just data ->
                    Ready data

                Nothing ->
                    NotFound

        _ ->
            NotReady id


findById : Int -> WebData (Dict Int a) -> Maybe a
findById id a =
    case a of
        Success a_ ->
            Dict.get id a_

        _ ->
            Nothing


emptyService : Service
emptyService =
    Service -1 "" "" ""


emptyConnection : Connection
emptyConnection =
    Connection -1 "" -1 -1 "" "" "" ""
