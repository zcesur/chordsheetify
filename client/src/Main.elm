module Main exposing (main)

import Api
import Browser
import Chart
import Chords exposing (Chord(..), Token(..), Voicing, parseChord)
import Chords.Note as Note
import Html exposing (Html, button, div, node, option, section, select, span, strong, text, textarea)
import Html.Attributes exposing (class, classList, disabled, placeholder, selected, spellcheck, value)
import Html.Events exposing (onClick, onInput, onMouseLeave, onMouseOver)
import Http
import Instrument exposing (Instrument(..))
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
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
    , parsedSheet : List Token
    , shift : Shift
    , instrument : Instrument
    , chord : Maybe Chord
    }


type Sheet
    = NewSheet String
    | LoadedSheet Api.Sheet


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    let
        storedSheet =
            flags
                |> Maybe.andThen (Decode.decodeString Decode.string >> Result.toMaybe)
                |> Maybe.withDefault ""
    in
    ( { sheet = NewSheet storedSheet
      , sheetList = Nothing
      , sheetId = Nothing
      , parsedSheet = parseSheet storedSheet
      , shift = Shift.fromInt 0
      , instrument = Guitar
      , chord = Nothing
      }
    , Api.getApiSheets GotSheetList
    )


parseSheet : String -> List Token
parseSheet =
    let
        isWordSymbol : Char -> Bool
        isWordSymbol c =
            Char.isAlphaNum c || isMusical c || not (isAscii c)

        isAscii : Char -> Bool
        isAscii char =
            Char.toCode char <= 0x7F

        isMusical : Char -> Bool
        isMusical c =
            List.member c [ '+', '-', '#', '/' ]

        consCompact : Token -> List Token -> List Token
        consCompact x ys =
            case ys of
                t :: ts ->
                    case ( x, t ) of
                        ( Lyrics a, Lyrics b ) ->
                            Lyrics (a ++ b) :: ts

                        _ ->
                            x :: ys

                [] ->
                    x :: ys
    in
    String.toList
        >> List.groupWhile (\x y -> xor (isWordSymbol x) (isWordSymbol y) |> not)
        >> List.map (fromNonEmpty >> String.fromList >> parseToken)
        >> List.foldr consCompact []


parseToken : String -> Token
parseToken s =
    s |> parseChord |> Result.map Parsed |> Result.withDefault (Lyrics s)


stringFromSheet : Sheet -> String
stringFromSheet sheet =
    case sheet of
        NewSheet s ->
            s

        LoadedSheet s ->
            s.content


sheetNonEmpty : Sheet -> Bool
sheetNonEmpty sheet =
    String.trim (stringFromSheet sheet) /= ""


transpose : Shift -> Chord -> Chord
transpose sh (Chord note quality) =
    Chord (Note.transpose (Shift.toInt sh) note) quality


chartFromChord : Instrument -> Chord -> Maybe ( Chord, Voicing )
chartFromChord instrument chord =
    let
        config =
            { tuning = Instrument.defaultTuning instrument, numFrets = 10 }
    in
    case Instrument.voicings instrument config chord of
        [] ->
            Nothing

        v :: _ ->
            Just ( chord, v )


chordsFromTokens : List Token -> List Chord
chordsFromTokens =
    let
        toChord token =
            case token of
                Parsed chord ->
                    Just chord

                _ ->
                    Nothing
    in
    List.filterMap toChord >> List.uniqueBy Chords.toString



-- UPDATE


type Msg
    = SetSheet String
    | SetSheetId (Maybe Api.SheetId)
    | SetInstrument String
    | SetChord (Maybe Chord)
    | Decremented
    | Incremented
    | GotSheet (Result Http.Error Api.Sheet)
    | GotSheetList (Result Http.Error (List Api.Sheet))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSheet x ->
            ( { model | sheet = NewSheet x, parsedSheet = parseSheet x, sheetId = Nothing }, saveSheet x )

        SetSheetId x ->
            ( { model | sheetId = x }
            , case x of
                Just id ->
                    Api.getApiSheetsById id GotSheet

                Nothing ->
                    Cmd.none
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


updateSheet : Model -> Api.Sheet -> Maybe Model
updateSheet model sheet =
    if Just sheet.id == model.sheetId then
        Just { model | sheet = LoadedSheet sheet, parsedSheet = parseSheet sheet.content }

    else
        Nothing


saveSheet : String -> Cmd Msg
saveSheet =
    Encode.string >> Encode.encode 0 >> Ports.storeSheet



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        renderIfSheetNonEmpty html =
            if sheetNonEmpty model.sheet then
                html

            else
                text ""
    in
    node "main"
        [ class "container" ]
        [ textarea
            [ classList [ ( "sheet-input", True ), ( "has-content", sheetNonEmpty model.sheet ) ]
            , placeholder "Paste a chord sheet or select one from the menu below."
            , value (stringFromSheet model.sheet)
            , onInput SetSheet
            , spellcheck False
            ]
            []
        , section [ class "row" ] (viewOptions model)
        , section [ class "charts" ] (viewCharts model) |> renderIfSheetNonEmpty
        , section [ class "sheet-output" ] (List.map (viewToken model.shift) model.parsedSheet) |> renderIfSheetNonEmpty
        ]


viewToken : Shift -> Token -> Html Msg
viewToken sh token =
    let
        el =
            case token of
                Lyrics s ->
                    text s

                Parsed chord ->
                    viewChord sh chord
    in
    span [] [ el ]


viewChord : Shift -> Chord -> Html Msg
viewChord sh x =
    x
        |> transpose sh
        |> Chords.toString
        |> text
        |> List.singleton
        |> strong [ onMouseOver (SetChord (Just x)), onMouseLeave (SetChord Nothing) ]


viewCharts : Model -> List (Html Msg)
viewCharts model =
    model.parsedSheet
        |> chordsFromTokens
        |> List.map (transpose model.shift)
        |> List.filterMap (chartFromChord model.instrument)
        |> List.map (viewChart model)


viewChart : Model -> ( Chord, Voicing ) -> Html Msg
viewChart model ( chord, voicing ) =
    let
        hoveredChord =
            Maybe.map (transpose model.shift >> Chords.toString) model.chord
    in
    Chart.view (Chords.toString chord) voicing
        |> List.singleton
        |> div
            [ classList
                [ ( "chart", True )
                , ( "active", Just (Chords.toString chord) == hoveredChord )
                ]
            ]


viewOptions : Model -> List (Html Msg)
viewOptions model =
    [ div [ class "column column-33" ] [ viewSheetOptions model ]
    , div [ class "column column-33" ] [ select [ onInput SetInstrument ] (List.map viewInstrumentOpt [ Guitar, Ukulele ]) ]
    , div [ class "column column-33" ]
        [ div [ class "button-group" ]
            [ button [ onClick Decremented ] [ text "-1" ]
            , button [ onClick Incremented ] [ text "+1" ]
            ]
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
                [ value (String.fromInt id), selected (selectedId == Just id) ]
                [ text name ]
    in
    case sheetList of
        Nothing ->
            select [ disabled True ] [ option [] [ text "Loading sheets..." ] ]

        Just sheets ->
            select
                [ onInput (String.toInt >> SetSheetId), disabled loading ]
                (option [ disabled True, selected (sheetId == Nothing) ] [ text "Select a sheet" ] :: List.map (viewSheetOpt sheetId) sheets)


viewInstrumentOpt : Instrument -> Html Msg
viewInstrumentOpt i =
    option
        [ value (Instrument.toString i) ]
        [ text (capitalize (Instrument.toString i)) ]



-- UTILITIES


capitalize : String -> String
capitalize s =
    case String.toList s of
        x :: xs ->
            (x |> String.fromChar |> String.toUpper) ++ String.fromList xs

        [] ->
            s


fromNonEmpty : ( a, List a ) -> List a
fromNonEmpty =
    uncurry (::)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( x, y ) =
    f x y