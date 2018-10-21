module Main exposing (..)

import Html exposing (Html)
import Browser exposing (element)
import Element exposing (..)
import Http exposing (Error)
import Element.Input as Input
import GraphQl
    exposing
        ( Operation
        , Variables
        , Query
        , Named
        )
import GraphQl.Http
import Json.Encode as Encode
import Json.Decode as Decode
    exposing
        ( Decoder
        , field
        , maybe
        , int
        , string
        , list
        )


endPointUrl : String
endPointUrl =
    "http://localhost:4000/api"



-- model


type alias Model =
    { searchWord : String
    , searchResult : List LegoColor
    }


type alias LegoColor =
    { name : String
    , blue : Int
    , green : Int
    , red : Int
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { searchWord = "tan", searchResult = [] }, Cmd.none )



-- json


decodeLegoColor : Decoder LegoColor
decodeLegoColor =
    Decode.map4 LegoColor
        (field "name" string)
        (field "blue" int)
        (field "green" int)
        (field "red" int)


decodeLegoColorList : Decoder (List LegoColor)
decodeLegoColorList =
    list decodeLegoColor



-- request


colorsRequest : Operation Query Variables
colorsRequest =
    GraphQl.named "query"
        [ GraphQl.field "colors"
            |> GraphQl.withArgument "matching" (GraphQl.variable "matching")
            |> GraphQl.withSelectors
                [ GraphQl.field "name"
                , GraphQl.field "blue"
                , GraphQl.field "green"
                , GraphQl.field "red"
                ]
        ]
        |> GraphQl.withVariables [ ( "matching", "String!" ) ]


sendRequest : String -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
sendRequest matching msg decoder =
    GraphQl.query colorsRequest
        |> GraphQl.addVariables [ ( "matching", Encode.string matching ) ]
        |> GraphQl.Http.send endPointUrl msg decoder



-- update


type Msg
    = GraphQlMsg (Result Error (List Color))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- subscriptions


subscriptions model =
    Sub.none



-- view


view : Model -> Html msg
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
