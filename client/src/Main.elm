module Main exposing (main)

import Api
import Browser
import Browser.Dom as Dom
import Button exposing (button)
import Chart
import Chords exposing (Chord(..), Token(..), Voicing)
import FeatherIcons as Icon
import Html exposing (Html, article, aside, div, main_, option, section, select, span, strong, text, textarea)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, selected, spellcheck, value)
import Html.Events exposing (onClick, onInput, onMouseLeave, onMouseOver)
import Http
import Instrument exposing (Instrument(..))
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Mode exposing (Mode(..))
import Ports
import Sheet exposing (Sheet(..))
import Shift exposing (Shift)
import Task



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


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    let
        sheet =
            flags
                |> Maybe.andThen (Decode.decodeString Decode.string >> Result.toMaybe)
                |> Maybe.withDefault ""
                |> NewSheet

        mode =
            if Sheet.isEmpty sheet then
                Edit

            else
                Preview
    in
    ( { sheet = sheet
      , sheetList = Nothing
      , sheetId = Nothing
      , parsedSheet = sheet |> Sheet.toString |> Sheet.parse
      , shift = Shift.fromInt 0
      , instrument = Guitar
      , chord = Nothing
      , mode = mode
      }
    , Api.getApiSheets GotSheetList
    )


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
        |> List.map (Shift.transpose model.shift)
        |> List.filterMap (chartFromChord model.instrument)



-- UPDATE


type Msg
    = NoOp
    | ChangedSheet String
    | SetSheetId (Maybe Api.SheetId)
    | SetInstrument String
    | HoveredOverChord (Maybe Chord)
    | SetMode Mode
    | Decremented
    | Incremented
    | GotSheet (Result Http.Error Api.Sheet)
    | GotSheetList (Result Http.Error (List Api.Sheet))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangedSheet x ->
            ( { model | sheet = NewSheet x, parsedSheet = Sheet.parse x, sheetId = Nothing }, saveSheet x )

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

        HoveredOverChord x ->
            ( { model | chord = x }, Cmd.none )

        SetMode x ->
            let
                cmd =
                    case x of
                        Preview ->
                            Cmd.none

                        Edit ->
                            Task.attempt (\_ -> NoOp) (Dom.focus editSheetAreaId)
            in
            ( { model | mode = x }, cmd )

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
                , parsedSheet = Sheet.parse sheet.content
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
    main_
        [ class "min-h-screen p-8 font-sans space-y-4 md:container mx-auto" ]
        [ section [ role "toolbar", class "sm:flex sm:space-x-4 sm:space-y-0 space-y-4" ] (viewOptions model)
        , aside
            [ class "charts grid gap-2 grid-cols-4 sm:grid-cols-6 lg:grid-cols-8 xl:grid-cols-10" ]
            (List.map (viewVoicing model) (voicings model))
            |> renderIf (voicings model |> List.isEmpty |> not)
        , article [ class "text-gray-900 whitespace-pre-wrap p-3" ]
            (List.map (viewToken model.shift) model.parsedSheet)
            |> renderIf (model.mode == Preview)
        , article [ class "border rounded shadow bg-white" ]
            [ textarea
                [ id editSheetAreaId
                , class "text-gray-900 p-3 bg-transparent w-full resize-none min-h-screen"
                , placeholder "Paste a chord sheet or select one from the menu."
                , ariaLabel "Edit chord sheet"
                , value (Sheet.toString model.sheet)
                , onInput ChangedSheet
                , spellcheck False
                ]
                []
            ]
            |> renderIf (model.mode == Edit)
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
        |> Shift.transpose sh
        |> Chords.toString
        |> text
        |> List.singleton
        |> strong [ role "tooltip", class "cursor-pointer hover:text-blue-500", onMouseOver (HoveredOverChord (Just x)), onMouseLeave (HoveredOverChord Nothing) ]


viewVoicing : Model -> ( Chord, Voicing ) -> Html Msg
viewVoicing model ( chord, voicing ) =
    let
        hoveredChord =
            Maybe.map (Shift.transpose model.shift >> Chords.toString) model.chord

        active =
            Just (Chords.toString chord) == hoveredChord
    in
    Chart.view (Chords.toString chord) voicing
        |> List.singleton
        |> div [ classList [ ( "chart", True ), ( "active rounded shadow-outline", active ) ] ]


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
                always NoOp

            else
                identity
    in
    [ btn [ onClick (Decremented |> adaptMsg), ariaLabel "Transpose left" ] [ Icon.chevronsLeft |> Icon.toHtml [] ]
    , btn [ onClick (Incremented |> adaptMsg), ariaLabel "Transpose right" ] [ Icon.chevronsRight |> Icon.toHtml [] ]
    ]


viewModeBtn : Sheet -> Mode -> Html Msg
viewModeBtn sheet newMode =
    let
        disabled =
            newMode == Preview && Sheet.isEmpty sheet

        msg =
            if disabled then
                NoOp

            else
                SetMode newMode
    in
    button "green"
        disabled
        [ onClick msg, ariaLabel (Mode.toString newMode) ]
        [ newMode |> Mode.toIcon |> Icon.toHtml [] ]


viewOptions : Model -> List (Html Msg)
viewOptions model =
    [ div [ class "w-full" ] [ viewSheetOptions model ]
    , div [ class "w-full" ] [ dropdown [ onInput SetInstrument, ariaLabel "Select instrument" ] (List.map viewInstrumentOpt [ Guitar, Ukulele ]) ]
    , div [ class "flex justify-center space-x-4" ]
        (List.concat
            [ viewShiftBtns model.parsedSheet
            , [ viewModeBtn model.sheet (Mode.toggle model.mode) ]
            ]
        )
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
            dropdown [ disabled True, ariaLabel "Select sheet" ] [ option [] [ text "Loading sheets..." ] ]

        Just sheets ->
            dropdown
                [ onInput (String.toInt >> SetSheetId), disabled loading, ariaLabel "Select sheet" ]
                (option [ disabled True, selected (sheetId == Nothing) ] [ text "Select a sheet" ] :: List.map (viewSheetOpt sheetId) sheets)


viewInstrumentOpt : Instrument -> Html Msg
viewInstrumentOpt i =
    option
        [ value (Instrument.toString i) ]
        [ text (capitalize (Instrument.toString i)) ]


editSheetAreaId : String
editSheetAreaId =
    "edit-sheet"


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


role : String -> Html.Attribute msg
role l =
    attribute "role" l


ariaLabel : String -> Html.Attribute msg
ariaLabel l =
    attribute "aria-label" l



-- UTILITIES


capitalize : String -> String
capitalize s =
    case String.toList s of
        x :: xs ->
            (x |> String.fromChar |> String.toUpper) ++ String.fromList xs

        [] ->
            s
