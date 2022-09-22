module App.View.Header exposing (Options, view)


import Html as H
import Html.Attributes as HA


type alias Options msg =
  { title : H.Html msg
  , scoreCard : H.Html msg
  }


view : Options msg -> H.Html msg
view { title, scoreCard } =
  H.header [ HA.class "header" ]
    [ H.div [ HA.class "header__item" ] [ title ]
    , H.div [ HA.class "header__item" ] [ scoreCard ]
    ]
