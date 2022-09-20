module App.View.Title exposing (view)


import Html as H
import Html.Attributes as HA


view : H.Html msg
view =
  H.h1 [ HA.class "title" ] [ H.text "2048" ]
