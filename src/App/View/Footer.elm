module App.View.Footer exposing (view)


import Html as H
import Html.Attributes as HA


view : H.Html msg
view =
  H.footer []
    [ viewHowToPlay
    , viewSeparator
    , viewNote
    , viewSeparator
    , viewCredits
    ]


viewHowToPlay : H.Html msg
viewHowToPlay =
  H.p [ HA.class "m0" ]
    [ H.strong [ HA.class "uppercase" ] [ H.text "How to play:" ]
    , H.text " Use your "
    , H.strong [] [ H.text "arrow keys" ]
    , H.text " to move the tiles. When two tiles with the same number touch, they "
    , H.strong [] [ H.text "merge into one!" ]
    ]


viewNote : H.Html msg
viewNote =
  H.p [ HA.class "m0" ]
    [ H.strong [ HA.class "uppercase" ] [ H.text "Note:" ]
    , H.text " This site is an "
    , viewLink
        { text = "Elm"
        , url = "https://elm-lang.org/"
        }
    , H.text " clone of the original 2048 JavaScript game that was created by "
    , viewLink
        { text = "Gabriele Cirulli"
        , url = "https://github.com/gabrielecirulli"
        }
    , H.text "."
    ]


viewCredits : H.Html msg
viewCredits =
  H.p [ HA.class "m0" ]
    [ H.text "Developed by "
    , viewLink
        { text = "Dwayne Crooks"
        , url = "https://github.com/dwayne"
        }
    , H.text ". 2048 is based on "
    , viewLink
        { text = "1024 by Veewo Studio"
        , url = "https://itunes.apple.com/us/app/1024!/id823499224"
        }
    , H.text " and conceptually similar to "
    , viewLink
        { text = "Threes by Asher Vollmer"
        , url = "http://asherv.com/threes/"
        }
    , H.text "."
    ]


viewLink : { text : String, url : String } -> H.Html msg
viewLink { text, url } =
  H.a [ HA.href url, HA.target "_blank" ] [ H.text text ]


viewSeparator : H.Html msg
viewSeparator =
  H.hr [ HA.class "separator" ] []
