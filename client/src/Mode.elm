module Mode exposing (Mode(..), toIcon, toggle)

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
