module App.View.ScoreCard exposing (ViewOptions, view)

import App.Data.Points exposing (Points)
import App.View.Score as Score
import Html as H
import Html.Attributes as HA


type alias ViewOptions msg =
    { current : Points
    , best : Points
    , state : Score.State
    , onChange : Score.Msg -> msg
    }


view : ViewOptions msg -> H.Html msg
view { current, best, state, onChange } =
    H.div [ HA.class "score-card" ]
        [ H.div [ HA.class "score-card__score" ]
            [ Score.viewCurrent
                { points = current
                , state = state
                , onChange = onChange
                }
            ]
        , H.div [ HA.class "score-card__score" ] [ Score.viewBest best ]
        ]
