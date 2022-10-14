module App.View.Button exposing (Options, view)


import App.Lib.Html.Attributes as HA
import Html as H
import Html.Attributes as HA
import Html.Events as HE


type alias Options msg =
  { id : String
  , text : String
  , onClick : msg
  }


view : Options msg -> H.Html msg
view { id, text, onClick } =
  let
    attrs =
      HA.toAttributes
        [ HA.class "button"
        , HE.onClick onClick
        ]
        [ if String.isEmpty id then
            Nothing

          else
            Just <| HA.id id
        ]
  in
  H.button attrs [ H.text text ]
