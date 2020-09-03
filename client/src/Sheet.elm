module Sheet exposing (Sheet(..), isEmpty, parse, toString)

import Api
import Chords exposing (Token(..), parseChord)
import List.Extra as List


type Sheet
    = NewSheet String
    | LoadedSheet Api.Sheet


parse : String -> List Token
parse =
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


toString : Sheet -> String
toString sheet =
    case sheet of
        NewSheet s ->
            s

        LoadedSheet s ->
            s.content


isEmpty : Sheet -> Bool
isEmpty sheet =
    String.trim (toString sheet) == ""


fromNonEmpty : ( a, List a ) -> List a
fromNonEmpty =
    uncurry (::)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( x, y ) =
    f x y
