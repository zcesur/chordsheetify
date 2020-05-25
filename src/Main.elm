module Main exposing (..)

import Browser
import Chart
import Chords exposing (Chord(..), Token(..))
import Chords.Note as Note exposing (Note)
import Html exposing (Html, button, div, node, option, section, select, span, strong, text, textarea)
import Html.Attributes exposing (class, classList, placeholder, spellcheck, value)
import Html.Events exposing (onClick, onInput, onMouseLeave, onMouseOver)
import Instrument exposing (Instrument(..))
import Instruments.Guitar as Guitar
import Instruments.Ukulele as Ukulele
import List exposing (concat, filterMap, map, singleton)
import List.Extra exposing (uniqueBy)
import Parser
import Shift exposing (Shift)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : RawSheet
    , output : ParsedSheet
    , shift : Shift
    , instrument : Instrument
    , chord : Maybe Chord
    }


init : Model
init =
    { input = "", output = [], shift = Shift.fromInt 0, instrument = Guitar, chord = Nothing }


type alias RawSheet =
    String


type alias ParsedLine =
    Result (List Parser.DeadEnd) (List Token)


type alias ParsedSheet =
    List ParsedLine


transpose : Shift -> Chord -> Chord
transpose sh (Chord note quality) =
    Chord (Note.transpose (Shift.toInt sh) note) quality


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
    map lineToChords >> concat >> uniqueBy Chords.toString



-- UPDATE


type Msg
    = SetSheet String
    | SetInstrument String
    | SetChord (Maybe Chord)
    | Decrement
    | Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetSheet x ->
            { model | input = x, output = Chords.parseSheet x }

        SetInstrument x ->
            { model | instrument = Maybe.withDefault model.instrument (Instrument.fromString x) }

        SetChord x ->
            { model | chord = x }

        Decrement ->
            { model | shift = Shift.decrement model.shift }

        Increment ->
            { model | shift = Shift.increment model.shift }



-- VIEW


view : Model -> Html Msg
view model =
    node "main"
        [ class "container" ]
        (concat
            [ [ textarea
                    [ classList [ ( "sheet-input", True ), ( "has-content", inputNonEmpty model ) ]
                    , placeholder textAreaPlaceholder
                    , value model.input
                    , onInput SetSheet
                    , spellcheck False
                    ]
                    []
              ]
            , map (renderIfInputNonEmpty model)
                [ section [ class "row" ] viewOptions
                , section [ class "charts" ] (viewCharts model)
                , section [ class "sheet-output" ] (map (viewLine model.shift) model.output)
                ]
            ]
        )


viewLine : Shift -> ParsedLine -> Html Msg
viewLine sh line =
    case line of
        Ok tokens ->
            div [] (map (viewToken sh) tokens)

        Err e ->
            span [] [ text (Parser.deadEndsToString e) ]


viewToken : Shift -> Token -> Html Msg
viewToken sh token =
    case token of
        Lyrics s ->
            span [] [ text s ]

        Parsed chord ->
            span [] [ viewChord sh chord ]


viewChord : Shift -> Chord -> Html Msg
viewChord sh x =
    x
        |> transpose sh
        |> Chords.toString
        |> text
        |> singleton
        |> strong [ onMouseOver (SetChord (Just x)), onMouseLeave (SetChord Nothing) ]


viewChart : Instrument -> Chord -> Html Msg
viewChart instrument chord =
    let
        config =
            { tuning = Instrument.defaultTuning instrument
            , numFrets = 10
            }

        name =
            Chords.toString chord
    in
    case Instrument.voicings instrument config chord of
        [] ->
            text ("Could not find voicing for chord " ++ name)

        first :: rest ->
            Chart.view name first


viewInstrumentOpt : Instrument -> Html Msg
viewInstrumentOpt i =
    option
        [ value (Instrument.toString i) ]
        [ text (capitalize (Instrument.toString i)) ]


viewCharts : Model -> List (Html Msg)
viewCharts model =
    model.output |> sheetToChords |> map (viewChordChart model)


viewChordChart : Model -> Chord -> Html Msg
viewChordChart model chord =
    chord
        |> transpose model.shift
        |> viewChart model.instrument
        |> singleton
        |> div
            [ classList
                [ ( "chart", True )
                , ( "active", Just (Chords.toString chord) == Maybe.map Chords.toString model.chord )
                ]
            ]


viewOptions : List (Html Msg)
viewOptions =
    [ div [ class "column column-33 column-offset-33" ] [ select [ onInput SetInstrument ] (map viewInstrumentOpt [ Guitar, Ukulele ]) ]
    , div [ class "column column-33" ]
        [ div [ class "button-group" ]
            [ button [ onClick Decrement ] [ text "-1" ]
            , button [ onClick Increment ] [ text "+1" ]
            ]
        ]
    ]


inputNonEmpty : Model -> Bool
inputNonEmpty { input } =
    String.trim input /= ""


renderIfInputNonEmpty : Model -> Html Msg -> Html Msg
renderIfInputNonEmpty model html =
    if inputNonEmpty model then
        html

    else
        text ""


capitalize : String -> String
capitalize s =
    case String.toList s of
        x :: xs ->
            (x |> String.fromChar |> String.toUpper) ++ String.fromList xs

        [] ->
            s


textAreaPlaceholder : String
textAreaPlaceholder =
    """Paste chord sheet.
Make sure that chords are surrounded with square brackets.

[G]               [Gsus2]  [G]             [G]     [Gsus2]  [G]   [C]    [C]    [C]    [C]
Such is the way of     the world, You can ne  -   ver know

[G]                 [Gsus2]  [G]              [G]   [Gsus2]  [G]   [C]     [C]    [C]    [C]
Just where to put all    your faith And how will   it  grow

      [D]         [G]                           [C]        [Cadd9]  [C]  [Cadd9]
Gonna rise up, Bringing back holes in dark memories

      [D]         [G]                [C]        [C]    [C]   [C6]
Gonna rise up, Turning mistakes into gold
"""
