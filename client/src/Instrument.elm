module Instrument exposing (Config, Instrument(..), defaultTuning, fromString, toString, voicings)

import Chords exposing (Chord, Voicing)
import Chords.Pitch exposing (Pitch)
import Instruments.Guitar as Guitar
import Instruments.Ukulele as Ukulele


type Instrument
    = Guitar
    | Ukulele


type alias Config =
    { tuning : List Pitch
    , numFrets : Int
    }


defaultTuning : Instrument -> List Pitch
defaultTuning instrument =
    case instrument of
        Guitar ->
            Guitar.defaultTuning

        Ukulele ->
            Ukulele.defaultTuning


voicings : Instrument -> Config -> Chord -> List Voicing
voicings instrument =
    case instrument of
        Guitar ->
            Guitar.voicings

        Ukulele ->
            Ukulele.voicings


fromString : String -> Maybe Instrument
fromString instrument =
    case instrument of
        "guitar" ->
            Just Guitar

        "ukulele" ->
            Just Ukulele

        _ ->
            Nothing


toString : Instrument -> String
toString instrument =
    case instrument of
        Guitar ->
            "guitar"

        Ukulele ->
            "ukulele"
