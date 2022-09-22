module App.View.Button exposing (Options, view)


import Html as H
import Html.Attributes as HA
import Html.Events as HE


type alias Options msg =
  { text : String
  , onClick : msg
  }


view : Options msg -> H.Html msg
view { text, onClick } =
  H.button
    [ HA.class "button"
    , HE.onClick onClick
    ]
    [ H.text text ]
