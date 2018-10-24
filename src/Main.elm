module Main exposing (..)

import Html exposing (Html)
import Browser exposing (element)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Element.Events as Events
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
    Decode.map5 LegoColor
        (field "id" string)
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
            |> GraphQl.withArgument
                "matching"
                (GraphQl.variable "matching")
            |> GraphQl.withSelectors
                [ GraphQl.field "id"
                , GraphQl.field "name"
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
    , mouseOverId : String
    }


type alias LegoColor =
    { id : String
    , name : String
    , blue : Int
    , green : Int
    , red : Int
    }


searchWord =
    "yellow"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchWord = searchWord
      , mouseOverId = ""
      , searchResult = []
      }
    , sendRequest searchWord GraphQlMsg decodeLegoColorList
    )



-- update


type Msg
    = GraphQlMsg (Result Error (List LegoColor))
    | InputText String
    | MouseEnter String
    | MouseLeave


errorColor =
    LegoColor "-5" "Error" 100 40 210


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

        InputText string ->
            ( { model | searchWord = string }
            , sendRequest string GraphQlMsg decodeLegoColorList
            )

        MouseLeave ->
            ( { model | mouseOverId = "" }
            , Cmd.none
            )

        MouseEnter id ->
            ( { model | mouseOverId = id }
            , Cmd.none
            )



-- subscriptions


subscriptions model =
    Sub.none



-- colors


makeGrey float =
    rgb float float float


white =
    makeGrey 1


black =
    makeGrey 0


lightGrey =
    makeGrey 0.7


charcoal =
    makeGrey 0.1


lightCharcoal =
    makeGrey 0.12


red =
    rgb 0.5 0.1 0.1



-- font helper


googleFont : String -> Attribute Msg
googleFont fontName =
    let
        fontString =
            String.replace " " "+" fontName
    in
        Font.family
            [ Font.external
                { url =
                    "https://fonts.googleapis.com/css?family="
                        ++ fontString
                , name = fontName
                }
            ]



-- view


box =
    el
        [ Background.color red
        , padding 10
        , centerX
        ]
        none


colorView : String -> LegoColor -> Element Msg
colorView mouseOverId color =
    let
        elementColor =
            rgb255 color.red color.green color.blue

        colorText string getter =
            let
                colorString =
                    color
                        |> getter
                        |> String.fromInt
            in
                row [ width fill ]
                    [ text <| string ++ ":  "
                    , el [ alignRight ] <| text colorString
                    ]

        belowElement =
            if color.id == mouseOverId then
                column
                    [ Background.color charcoal
                    , moveLeft 10
                    , padding 10
                    , spacing 5
                    , Border.widthEach
                        { bottom = 5
                        , left = 5
                        , right = 5
                        , top = 0
                        }
                    , Border.color black
                    ]
                    [ colorText "Red" .red
                    , colorText "Green" .green
                    , colorText "Blue" .blue
                    ]
            else
                none

        boxAttributes =
            [ Background.color elementColor
            , width <| px 70
            , height <| px 70
            ]

        rowAttributes =
            [ spacing 10
            , width <| px 300
            , height <| px 70
            , Background.color charcoal
            , Border.width 2
            , Border.color black
            , mouseOver [ Border.color lightCharcoal ]
            , Events.onMouseEnter <| MouseEnter color.id
            , Events.onMouseLeave MouseLeave
            ]
    in
        row rowAttributes
            [ el boxAttributes none
            , el
                [ below belowElement
                , width fill
                ]
              <|
                paragraph
                    [ padding 5 ]
                    [ text color.name ]
            ]


inputBox model label msg =
    el [ padding 10 ] <|
        Input.text
            [ width <| px 250
            , Font.color black
            ]
            { onChange = (\x -> msg x)
            , text = model.searchWord
            , placeholder = Nothing
            , label =
                Input.labelLeft [ moveDown 13 ] <| text label
            }


view : Model -> Html Msg
view model =
    layout
        [ Background.color black
        , Font.color lightGrey
        , googleFont "Noto Serif"
        ]
    <|
        column
            [ padding 20
            , spacing 10
            ]
        <|
            [ inputBox model "Part of color name: " InputText
            , wrappedRow
                [ spacing 10
                ]
                (model.searchResult
                    |> List.map (colorView model.mouseOverId)
                )
            ]



-- main


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
