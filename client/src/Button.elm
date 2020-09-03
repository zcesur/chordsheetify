module Button exposing (button)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (class)


mkClass : List String -> String
mkClass =
    List.intersperse " " >> String.concat


button : String -> Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
button color disabled attributes children =
    let
        color_ =
            if disabled then
                "gray"

            else
                color

        condCls =
            if not disabled then
                [ "cursor-pointer", "hover:bg-" ++ color ++ "-600" ]

            else
                [ "cursor-not-allowed" ]

        commonCls =
            [ "shadow rounded py-2 px-3 border text-white"
            , "bg-" ++ color_ ++ "-500"
            , "border-" ++ color_ ++ "-500"
            ]
    in
    Html.button ((commonCls ++ condCls |> mkClass |> class) :: attributes) children
