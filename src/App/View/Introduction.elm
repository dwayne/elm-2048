module App.View.Introduction exposing (view)


import App.View.Button as Button
import Html as H
import Html.Attributes as HA


view : msg -> H.Html msg
view onNewGame =
  H.div [ HA.class "introduction" ]
    [ H.div [ HA.class "introduction__paragraph" ]
        [ H.p [ HA.class "m0" ]
            [ H.text "Join the numbers and get to the "
            , H.strong [ HA.class "nowrap" ] [ H.text "2048 tile!" ]
            ]
        ]
    , H.div [ HA.class "introduction__button" ]
        [ Button.view
            { id = ""
            , text = "New Game"
            , onClick = onNewGame
            }
        ]
    ]
