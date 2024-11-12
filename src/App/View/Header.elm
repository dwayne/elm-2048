module App.View.Header exposing (ViewOptions, view)

import App.View.ScoreCard as ScoreCard
import App.View.Title as Title
import Html as H
import Html.Attributes as HA


type alias ViewOptions msg =
    ScoreCard.ViewOptions msg


view : ViewOptions msg -> H.Html msg
view options =
    H.header [ HA.class "header" ]
        [ H.div [ HA.class "header__title" ] [ Title.view ]
        , H.div [ HA.class "header__score-card" ] [ ScoreCard.view options ]
        ]
