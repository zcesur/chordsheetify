module Main exposing (main)

import Api
import Browser
import Button exposing (button)
import Chart
import Chords exposing (Chord(..), Token(..), Voicing, parseChord)
import Chords.Note as Note
import FeatherIcons as Icon
import Html exposing (Html, div, node, option, section, select, span, strong, text, textarea)
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
    , mode : Mode
    }


type Sheet
    = NewSheet String
    | LoadedSheet Api.Sheet


type Mode
    = Edit
    | Preview


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    let
        sheet =
            flags
                |> Maybe.andThen (Decode.decodeString Decode.string >> Result.toMaybe)
                |> Maybe.withDefault ""
                |> NewSheet

        mode =
            if sheetNonEmpty sheet then
                Preview

            else
                Edit
    in
    ( { sheet = sheet
      , sheetList = Nothing
      , sheetId = Nothing
      , parsedSheet = sheet |> stringFromSheet |> parseSheet
      , shift = Shift.fromInt 0
      , instrument = Guitar
      , chord = Nothing
      , mode = mode
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


voicings : Model -> List ( Chord, Voicing )
voicings model =
    model.parsedSheet
        |> chordsFromTokens
        |> List.map (transpose model.shift)
        |> List.filterMap (chartFromChord model.instrument)


toggleMode : Mode -> Mode
toggleMode m =
    case m of
        Edit ->
            Preview

        Preview ->
            Edit


iconFromMode : Mode -> Icon.Icon
iconFromMode m =
    case m of
        Edit ->
            Icon.edit2

        Preview ->
            Icon.eye



-- UPDATE


type Msg
    = Noop
    | SetSheet String
    | SetSheetId (Maybe Api.SheetId)
    | SetInstrument String
    | SetChord (Maybe Chord)
    | SetMode Mode
    | Decremented
    | Incremented
    | GotSheet (Result Http.Error Api.Sheet)
    | GotSheetList (Result Http.Error (List Api.Sheet))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

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

        SetMode x ->
            ( { model | mode = x }, Cmd.none )

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
        Just
            { model
                | sheet = LoadedSheet sheet
                , parsedSheet = parseSheet sheet.content
                , mode = Preview
                , shift = Shift.fromInt 0
            }

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
        renderIf p html =
            if p then
                html

            else
                text ""
    in
    node "main"
        [ class "min-h-screen p-8 font-sans space-y-4 md:container mx-auto" ]
        [ section [ class "sm:flex sm:space-x-4 sm:space-y-0 space-y-4" ] (viewOptions model)
        , section [ class "charts flex flex-wrap" ] (List.map (viewVoicing model) (voicings model)) |> renderIf (not <| List.isEmpty (voicings model))
        , section [ class "border rounded shadow bg-white" ]
            [ textarea
                [ class "text-gray-900 p-3 bg-transparent w-full resize-none min-h-screen"
                , placeholder "Paste a chord sheet or select one from the menu below."
                , value (stringFromSheet model.sheet)
                , onInput SetSheet
                , spellcheck False
                ]
                []
            ]
            |> renderIf (model.mode == Edit)
        , section [ class "preview-area text-gray-900 whitespace-pre-wrap p-3" ] (List.map (viewToken model.shift) model.parsedSheet) |> renderIf (model.mode == Preview)
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


viewVoicing : Model -> ( Chord, Voicing ) -> Html Msg
viewVoicing model ( chord, voicing ) =
    let
        hoveredChord =
            Maybe.map (transpose model.shift >> Chords.toString) model.chord

        active =
            Just (Chords.toString chord) == hoveredChord
    in
    Chart.view (Chords.toString chord) voicing
        |> List.singleton
        |> div [ class "chart", classList [ ( "active rounded shadow-outline", active ) ] ]


viewShiftBtns : List Token -> List (Html Msg)
viewShiftBtns parsedSheet =
    let
        color =
            "blue"

        disabled =
            parsedSheet |> chordsFromTokens |> List.isEmpty

        btn =
            button color disabled

        adaptMsg =
            if disabled then
                always Noop

            else
                identity
    in
    [ btn [ onClick (Decremented |> adaptMsg) ] [ Icon.minus |> Icon.toHtml [] ]
    , btn [ onClick (Incremented |> adaptMsg) ] [ Icon.plus |> Icon.toHtml [] ]
    ]


viewModeBtn : Sheet -> Mode -> Html Msg
viewModeBtn sheet newMode =
    button "green"
        (newMode == Preview && not (sheetNonEmpty sheet))
        [ onClick (SetMode newMode) ]
        [ newMode |> iconFromMode |> Icon.toHtml [] ]


viewOptions : Model -> List (Html Msg)
viewOptions model =
    [ div [ class "w-full" ] [ viewSheetOptions model ]
    , div [ class "w-full" ] [ dropdown [ onInput SetInstrument ] (List.map viewInstrumentOpt [ Guitar, Ukulele ]) ]
    , div []
        [ div [ class "flex justify-center space-x-4" ]
            (List.concat
                [ viewShiftBtns model.parsedSheet
                , [ viewModeBtn model.sheet (toggleMode model.mode) ]
                ]
            )
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
            dropdown [ disabled True ] [ option [] [ text "Loading sheets..." ] ]

        Just sheets ->
            dropdown
                [ onInput (String.toInt >> SetSheetId), disabled loading ]
                (option [ disabled True, selected (sheetId == Nothing) ] [ text "Select a sheet" ] :: List.map (viewSheetOpt sheetId) sheets)


viewInstrumentOpt : Instrument -> Html Msg
viewInstrumentOpt i =
    option
        [ value (Instrument.toString i) ]
        [ text (capitalize (Instrument.toString i)) ]


dropdown : List (Html.Attribute msg) -> List (Html msg) -> Html msg
dropdown attributes children =
    let
        cls =
            "shadow appearance-none border rounded w-full py-2 px-3 bg-white text-gray-800 cursor-pointer"
    in
    div [ class "relative" ]
        [ select (class cls :: attributes) children
        , div
            [ class "pointer-events-none absolute inset-y-0 right-0 flex items-center px-2 text-gray-700" ]
            [ Icon.chevronDown |> Icon.withSize 16 |> Icon.toHtml [] ]
        ]



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
