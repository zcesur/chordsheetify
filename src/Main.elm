module Main exposing (..)

import Browser
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String }


init : Model
init =
    { content = "" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ textarea [ class "sheet-input", placeholder "Sheet", value model.content, onInput Change ] []
        , div [ class "sheet-output" ] [ text model.content ]
        ]
