module App.View.Message exposing
  ( GameOverOptions, viewGameOver
  , WinOptions, viewWin
  )


import App.View.Button as Button
import Html as H
import Html.Attributes as HA


type alias GameOverOptions msg =
  { onTryAgain : msg
  }


viewGameOver : GameOverOptions msg -> H.Html msg
viewGameOver { onTryAgain } =
  H.div [ HA.class "message" ]
    [ H.h2 [ HA.class "message__title" ] [ H.text "Game over!" ]
    , H.div [ HA.class "message__controls" ]
        [ H.div [ HA.class "message__button" ]
            [ Button.view
                { text = "Try again"
                , onClick = onTryAgain
                }
            ]
        ]
    ]


type alias WinOptions msg =
  { onTryAgain : msg
  , onKeepPlaying : msg
  }


viewWin : WinOptions msg -> H.Html msg
viewWin { onTryAgain, onKeepPlaying } =
  H.div [ HA.class "message message--success" ]
    [ H.h2 [ HA.class "message__title" ] [ H.text "You win!" ]
    , H.div [ HA.class "message__controls" ]
        [ H.div [ HA.class "message__button" ]
            [ Button.view
                { text = "Keep going"
                , onClick = onKeepPlaying
                }
            ]
        , H.div [ HA.class "message__button" ]
            [ Button.view
                { text = "Try again"
                , onClick = onTryAgain
                }
            ]
        ]
    ]
