module Main exposing (..)

import Browser
import Chords exposing (Chord(..), Token(..))
import Chords.Note exposing (Note)
import Html exposing (Html, button, div, span, strong, text, textarea)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Parser exposing (..)
import Shift exposing (Shift)



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
    { input : RawSheet, output : ParsedSheet, shift : Shift }


init : Model
init =
    { input = "", output = [], shift = Shift.fromInt 0 }



-- UPDATE


type Msg
    = Change String
    | Decrement
    | Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change i ->
            { model | input = i, output = Chords.parseSheet i }

        Decrement ->
            { model | shift = Shift.decrement model.shift }

        Increment ->
            { model | shift = Shift.increment model.shift }



-- VIEW


transpose : Shift -> Chord -> Chord
transpose sh (Chord note quality) =
    Chord (Chords.Note.transpose (Shift.toInt sh) note) quality


renderLine : Shift -> ParsedLine -> Html Msg
renderLine sh line =
    case line of
        Ok tokens ->
            div [] (List.map (renderToken sh) tokens)

        Err e ->
            span [] [ text (deadEndsToString e) ]


renderToken : Shift -> Token -> Html Msg
renderToken sh token =
    case token of
        Lyrics s ->
            span [] [ text s ]

        Parsed chord ->
            span [] [ renderChord sh chord ]


renderChord : Shift -> Chord -> Html Msg
renderChord sh =
    transpose sh >> Chords.toString >> text >> List.singleton >> strong []


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ textarea [ class "sheet-input", placeholder "Sheet", value model.input, onInput Change ] []
        , div [ class "button-group" ]
            [ button [ class "button-outline", onClick Decrement ] [ text "-1" ]
            , button [ class "button-outline", onClick Increment ] [ text "+1" ]
            ]
        , div [ class "sheet-output" ] (List.map (renderLine model.shift) model.output)
        ]
