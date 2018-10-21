module Main exposing (..)

import Html exposing (Html)
import Browser exposing (element)
import Element exposing (..)
import Element.Background as Background
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
    field "colors"
        (list decodeLegoColor)



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


sendRequest :
    String
    -> (Result Http.Error a -> msg)
    -> Decoder a
    -> Cmd msg
sendRequest matching msg decoder =
    GraphQl.query colorsRequest
        |> GraphQl.addVariables [ ( "matching", Encode.string matching ) ]
        |> GraphQl.Http.send endPointUrl msg decoder



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


searchWord =
    "blue"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchWord = searchWord, searchResult = [] }
    , sendRequest searchWord GraphQlMsg decodeLegoColorList
    )



-- update


type Msg
    = GraphQlMsg (Result Error (List LegoColor))


errorColor =
    LegoColor "Error" 100 40 210


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GraphQlMsg (Ok result) ->
            ( { model | searchResult = result }
            , Cmd.none
            )

        GraphQlMsg (Err _) ->
            ( { model | searchResult = [ errorColor ] }
            , Cmd.none
            )



-- subscriptions


subscriptions model =
    Sub.none



-- view


colorView : LegoColor -> Element Msg
colorView color =
    let
        elementColor =
            rgb255 color.red color.green color.blue

        boxAttributes =
            [ Background.color elementColor
            , width <| px 50
            , height <| px 50
            ]
    in
        row [ spacing 10 ] [ el boxAttributes none, text color.name ]


view : Model -> Html Msg
view model =
    layout [] <|
        column
            [ padding 20
            , spacing 10
            ]
        <|
            List.map colorView model.searchResult



-- main


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
