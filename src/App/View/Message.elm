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
  view
    { isSuccess = False
    , title = "Game over!"
    , buttonOptions =
        [ { text = "Try again"
          , onClick = onTryAgain
          }
        ]
    }


type alias WinOptions msg =
  { onKeepPlaying : msg
  , onTryAgain : msg
  }


viewWin : WinOptions msg -> H.Html msg
viewWin { onKeepPlaying, onTryAgain } =
  view
    { isSuccess = True
    , title = "You win!"
    , buttonOptions =
        [ { text = "Keep going"
          , onClick = onKeepPlaying
          }
        , { text = "Try again"
          , onClick = onTryAgain
          }
        ]
    }


type alias Options msg =
  { isSuccess : Bool
  , title : String
  , buttonOptions : List (Button.Options msg)
  }


view : Options msg -> H.Html msg
view { isSuccess, title, buttonOptions } =
  H.div
    [ HA.class "message"
    , HA.classList
        [ ("message--success", isSuccess)
        ]
    ]
    [ H.h2 [ HA.class "message__title" ] [ H.text title ]
    , H.div [ HA.class "message__controls" ] <|
        List.map viewMessageButton buttonOptions
    ]


viewMessageButton : Button.Options msg -> H.Html msg
viewMessageButton options =
  H.div [ HA.class "message__button" ] [ Button.view options ]
