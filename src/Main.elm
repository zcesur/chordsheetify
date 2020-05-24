module Main exposing (..)

import Browser
import Chords exposing (Chord(..), Token(..))
import Chords.Chart
import Chords.Note exposing (Note)
import Html exposing (Html, button, div, span, strong, text, textarea)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Instruments.Guitar as Guitar
import List exposing (concatMap, filterMap, map, singleton)
import List.Extra exposing (uniqueBy)
import Parser exposing (DeadEnd, deadEndsToString)
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


transpose : Shift -> Chord -> Chord
transpose sh (Chord note quality) =
    Chord (Chords.Note.transpose (Shift.toInt sh) note) quality


toChord : Token -> Maybe Chord
toChord token =
    case token of
        Lyrics _ ->
            Nothing

        Parsed chord ->
            Just chord


lineToChords : ParsedLine -> List Chord
lineToChords line =
    case line of
        Ok tokens ->
            filterMap toChord tokens

        Err e ->
            []


sheetToChords : ParsedSheet -> List Chord
sheetToChords =
    concatMap lineToChords >> uniqueBy Chords.toString


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


viewLine : Shift -> ParsedLine -> Html Msg
viewLine sh line =
    case line of
        Ok tokens ->
            div [] (map (viewToken sh) tokens)

        Err e ->
            span [] [ text (deadEndsToString e) ]


viewToken : Shift -> Token -> Html Msg
viewToken sh token =
    case token of
        Lyrics s ->
            span [] [ text s ]

        Parsed chord ->
            span [] [ viewChord sh chord ]


viewChord : Shift -> Chord -> Html Msg
viewChord sh =
    transpose sh >> Chords.toString >> text >> singleton >> strong []


viewChart : Chord -> Html Msg
viewChart chord =
    let
        config =
            { tuning = Guitar.defaultTuning
            , numFrets = 10
            }

        name =
            Chords.toString chord
    in
    case Guitar.voicings config chord of
        [] ->
            Html.span []
                [ Html.text
                    ("Could not find voicing for chord " ++ name)
                ]

        first :: rest ->
            Chords.Chart.view name first


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ textarea [ class "sheet-input", placeholder "Sheet", value model.input, onInput Change ] []
        , div [ class "button-group" ]
            [ button [ class "button-outline", onClick Decrement ] [ text "-1" ]
            , button [ class "button-outline", onClick Increment ] [ text "+1" ]
            ]
        , div [] (model.output |> sheetToChords |> map (transpose model.shift) |> map viewChart)
        , div [ class "sheet-output" ] (map (viewLine model.shift) model.output)
        ]
