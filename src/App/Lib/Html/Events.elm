module App.Lib.Html.Events exposing (onAnimationEnd, onAnimationStart)

import Html as H
import Html.Events as HE
import Json.Decode as JD


onAnimationStart : msg -> H.Attribute msg
onAnimationStart msg =
    HE.on "animationstart" <| JD.succeed msg


onAnimationEnd : msg -> H.Attribute msg
onAnimationEnd msg =
    HE.on "animationend" <| JD.succeed msg
