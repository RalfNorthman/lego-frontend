module Main exposing (..)

import Browser exposing (element)
import Element exposing (..)
import Element.Input as Input
import Graphql.Http
import RemoteData exposing (RemoteData)


-- import Graphql.OptionalArgument exposing (OptionalArgument(..))
-- graphql


type alias Response =
    List String


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error Response) Response)

query =
  

makeRequest =
    query
        |> Graphql.Http.queryRequest "http://localhost:4000/api"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)



-- model


type alias Model =
    { searchWord : String, searchResult : String }


init : () -> ( Model, Cmd msg )
init _ =
    ( { searchWord = "tan", searchResult = "" }, Cmd.none )



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
