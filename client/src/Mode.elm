module Mode exposing (Mode(..), toIcon, toString, toggle)

import FeatherIcons as Icon


type Mode
    = Edit
    | Preview


toggle : Mode -> Mode
toggle m =
    case m of
        Edit ->
            Preview

        Preview ->
            Edit


toIcon : Mode -> Icon.Icon
toIcon m =
    case m of
        Edit ->
            Icon.edit3

        Preview ->
            Icon.eye


toString : Mode -> String
toString m =
    case m of
        Edit ->
            "Edit"

        Preview ->
            "Preview"
