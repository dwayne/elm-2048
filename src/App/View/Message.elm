module App.View.Message exposing
    ( GameOverOptions
    , WinOptions
    , viewGameOver
    , viewWin
    )

import App.Lib.Html.Events as HE
import App.View.Button as Button
import Html as H
import Html.Attributes as HA


type alias GameOverOptions msg =
    { id : String
    , onTryAgain : msg
    , onOpen : msg
    }


viewGameOver : GameOverOptions msg -> H.Html msg
viewGameOver { id, onTryAgain, onOpen } =
    view
        { isSuccess = False
        , title = "Game over!"
        , buttonOptions =
            [ { id = id
              , text = "Try again"
              , onClick = onTryAgain
              }
            ]
        , onOpen = onOpen
        }


type alias WinOptions msg =
    { id : String
    , onKeepPlaying : msg
    , onTryAgain : msg
    , onOpen : msg
    }


viewWin : WinOptions msg -> H.Html msg
viewWin { id, onKeepPlaying, onTryAgain, onOpen } =
    view
        { isSuccess = True
        , title = "You win!"
        , buttonOptions =
            [ { id = id
              , text = "Keep going"
              , onClick = onKeepPlaying
              }
            , { id = ""
              , text = "Try again"
              , onClick = onTryAgain
              }
            ]
        , onOpen = onOpen
        }


type alias Options msg =
    { isSuccess : Bool
    , title : String
    , buttonOptions : List (Button.Options msg)
    , onOpen : msg
    }


view : Options msg -> H.Html msg
view { isSuccess, title, buttonOptions, onOpen } =
    H.div
        [ HA.class "message"
        , HA.classList
            [ ( "message--success", isSuccess )
            ]
        , HE.onAnimationStart onOpen
        ]
        [ H.h2 [ HA.class "message__title" ] [ H.text title ]
        , H.div [ HA.class "message__buttons" ] <|
            List.map viewMessageButton buttonOptions
        ]


viewMessageButton : Button.Options msg -> H.Html msg
viewMessageButton options =
    H.div [ HA.class "message__button" ] [ Button.view options ]
