module Main exposing (main)

import Api
import Browser
import Chart
import Chords exposing (Chord(..), Token(..))
import Chords.Note as Note
import Html exposing (Html, button, div, node, option, section, select, span, strong, text, textarea)
import Html.Attributes exposing (class, classList, disabled, placeholder, selected, spellcheck, value)
import Html.Events exposing (onClick, onInput, onMouseLeave, onMouseOver)
import Http
import Instrument exposing (Instrument(..))
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (concat, filterMap, map, singleton)
import List.Extra exposing (uniqueBy)
import Parser
import Ports
import Shift exposing (Shift)



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { sheet : Sheet
    , sheetList : Maybe (List Api.Sheet)
    , sheetId : Maybe Api.SheetId
    , parsedSheet : ParsedSheet
    , shift : Shift
    , instrument : Instrument
    , chord : Maybe Chord
    }


type Sheet
    = NewSheet String
    | LoadedSheet Api.Sheet


type alias ParsedLine =
    Result (List Parser.DeadEnd) (List Token)


type alias ParsedSheet =
    List ParsedLine


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    let
        storedSheet =
            flags |> Maybe.andThen decodeStoredSheet |> Maybe.withDefault ""
    in
    ( { sheet = NewSheet storedSheet
      , sheetList = Nothing
      , sheetId = Nothing
      , parsedSheet = Chords.parseSheet storedSheet
      , shift = Shift.fromInt 0
      , instrument = Guitar
      , chord = Nothing
      }
    , Api.getApiSheets GotSheetList
    )


decodeStoredSheet : String -> Maybe String
decodeStoredSheet =
    Decode.decodeString Decode.string >> Result.toMaybe


content : Sheet -> String
content sheet =
    case sheet of
        NewSheet s ->
            s

        LoadedSheet s ->
            s.content


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

        Err _ ->
            []


sheetToChords : ParsedSheet -> List Chord
sheetToChords =
    map lineToChords >> concat >> uniqueBy Chords.toString


saveSheet : String -> Cmd Msg
saveSheet =
    Encode.string >> Encode.encode 0 >> Ports.storeSheet



-- UPDATE


type Msg
    = SetSheet String
    | SetSheetId Api.SheetId
    | SetInstrument String
    | SetChord (Maybe Chord)
    | Decremented
    | Incremented
    | GotSheet (Result Http.Error Api.Sheet)
    | GotSheetList (Result Http.Error (List Api.Sheet))


updateSheet : Model -> Api.Sheet -> Maybe Model
updateSheet model sheet =
    if Just sheet.id == model.sheetId then
        Just { model | sheet = LoadedSheet sheet, parsedSheet = Chords.parseSheet sheet.content }

    else
        Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSheet x ->
            ( { model | sheet = NewSheet x, parsedSheet = Chords.parseSheet x, sheetId = Nothing }, saveSheet x )

        SetSheetId x ->
            ( { model | sheetId = Just x }
            , Api.getApiSheetsById x GotSheet
            )

        SetInstrument x ->
            ( { model | instrument = Maybe.withDefault model.instrument (Instrument.fromString x) }, Cmd.none )

        SetChord x ->
            ( { model | chord = x }, Cmd.none )

        Decremented ->
            ( { model | shift = Shift.decrement model.shift }, Cmd.none )

        Incremented ->
            ( { model | shift = Shift.increment model.shift }, Cmd.none )

        GotSheet result ->
            result
                |> Result.toMaybe
                |> Maybe.andThen (updateSheet model)
                |> Maybe.withDefault model
                |> (\m -> ( m, Cmd.none ))

        GotSheetList result ->
            case result of
                Ok xs ->
                    ( { model | sheetList = Just xs }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    node "main"
        [ class "container" ]
        (concat
            [ [ textarea
                    [ classList [ ( "sheet-input", True ), ( "has-content", sheetNonEmpty model.sheet ) ]
                    , placeholder textAreaPlaceholder
                    , value (content model.sheet)
                    , onInput SetSheet
                    , spellcheck False
                    ]
                    []
              ]
            , map (renderIfSheetNonEmpty model.sheet)
                [ section [ class "row" ] (viewOptions model)
                , section [ class "charts" ] (viewCharts model)
                , section [ class "sheet-output" ] (map (viewLine model.shift) model.parsedSheet)
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

        first :: _ ->
            Chart.view name first


viewInstrumentOpt : Instrument -> Html Msg
viewInstrumentOpt i =
    option
        [ value (Instrument.toString i) ]
        [ text (capitalize (Instrument.toString i)) ]


viewCharts : Model -> List (Html Msg)
viewCharts model =
    model.parsedSheet |> sheetToChords |> map (viewChordChart model)


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


viewSheetOptions : Model -> Html Msg
viewSheetOptions { sheet, sheetId, sheetList } =
    let
        loading =
            case ( sheet, sheetId ) of
                ( _, Nothing ) ->
                    False

                ( NewSheet _, Just _ ) ->
                    True

                ( LoadedSheet s, Just id ) ->
                    s.id /= id

        viewSheetOpt selectedId { id, name } =
            option
                [ value id, selected (selectedId == Just id) ]
                [ text name ]
    in
    case sheetList of
        Nothing ->
            select [ disabled True ] [ option [] [ text "Loading sheets..." ] ]

        Just sheets ->
            select
                [ onInput SetSheetId, disabled loading ]
                (option [ disabled True, selected (sheetId == Nothing) ] [ text "Select a sheet" ] :: map (viewSheetOpt sheetId) sheets)


viewOptions : Model -> List (Html Msg)
viewOptions model =
    [ div [ class "column column-33" ] [ viewSheetOptions model ]
    , div [ class "column column-33" ] [ select [ onInput SetInstrument ] (map viewInstrumentOpt [ Guitar, Ukulele ]) ]
    , div [ class "column column-33" ]
        [ div [ class "button-group" ]
            [ button [ onClick Decremented ] [ text "-1" ]
            , button [ onClick Incremented ] [ text "+1" ]
            ]
        ]
    ]


sheetNonEmpty : Sheet -> Bool
sheetNonEmpty sheet =
    String.trim (content sheet) /= ""


renderIfSheetNonEmpty : Sheet -> Html Msg -> Html Msg
renderIfSheetNonEmpty sheet html =
    if sheetNonEmpty sheet then
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
