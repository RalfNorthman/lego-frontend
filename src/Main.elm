module Main exposing (..)

import Browser exposing (element)
import Element exposing (..)
import Element.Input as Input


-- model


type alias Model =
    Int


init : () -> ( Model, Cmd msg )
init _ =
    ( 0, Cmd.none )



-- update


update model msg =
    ( model, Cmd.none )



-- subscriptions


subscriptions model =
    Sub.none



-- view


view model =
    layout [] <|
        text "Work in progress."



-- main


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
