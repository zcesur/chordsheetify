module Main exposing (..)

import Browser
import Chords exposing (Token(..))
import Html exposing (Html, b, div, span, text, textarea)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)
import Parser exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias RawSheet =
    String


type alias ParsedLine =
    Result (List DeadEnd) (List Token)


type alias ParsedSheet =
    List ParsedLine


type alias Model =
    { input : RawSheet, output : ParsedSheet }


init : Model
init =
    { input = "", output = [] }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change i ->
            { model | input = i, output = Chords.parseSheet i }



-- VIEW


renderLine : ParsedLine -> Html Msg
renderLine res =
    case res of
        Ok ts ->
            div [] (List.map renderToken ts)

        Err e ->
            span [] [ text (deadEndsToString e) ]


renderToken : Token -> Html Msg
renderToken t =
    case t of
        Lyrics s ->
            span [] [ text s ]

        Parsed c ->
            span [] [ b [] [ text (Chords.toString c) ] ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ textarea [ class "sheet-input", placeholder "Sheet", value model.input, onInput Change ] []
        , div [ class "sheet-output" ] (List.map renderLine model.output)
        ]
