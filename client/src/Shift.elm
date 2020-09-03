module Shift exposing (Shift, decrement, fromInt, increment, transpose)

import Chords exposing (Chord(..), Token(..))
import Chords.Note as Note


type Shift
    = Shift Int


increment : Shift -> Shift
increment (Shift x) =
    case x of
        11 ->
            Shift 0

        i ->
            Shift (i + 1)


decrement : Shift -> Shift
decrement (Shift x) =
    case x of
        0 ->
            Shift 11

        i ->
            Shift (i - 1)


fromInt : Int -> Shift
fromInt x =
    if x < 0 then
        fromInt (x + 12)

    else if x < 12 then
        Shift x

    else
        fromInt (x - 12)


transpose : Shift -> Chord -> Chord
transpose (Shift x) (Chord note quality) =
    Chord (Note.transpose x note) quality
