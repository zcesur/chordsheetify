module Shift exposing (Shift, decrement, fromInt, increment, toInt, toString)


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


toInt : Shift -> Int
toInt (Shift x) =
    x


toString : Shift -> String
toString (Shift x) =
    String.fromInt x
