module App.Lib.Ease exposing (inOut)

import Ease


inOut : Float -> Float
inOut =
    -- See https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function
    -- ease-in-out
    Ease.bezier 0.42 0.0 0.58 1.0
