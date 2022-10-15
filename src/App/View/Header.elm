module App.View.Header exposing (Options, view)

import App.Data.Points exposing (Points)
import App.Data.Tally exposing (Reckoning)
import App.View.ScoreCard as ScoreCard
import App.View.Title as Title
import Html as H
import Html.Attributes as HA


type alias Options msg =
    { reckoning : Reckoning
    , state : ScoreCard.State
    , onChange : ScoreCard.Msg -> msg
    }


view : Options msg -> H.Html msg
view { reckoning, state, onChange } =
    H.header [ HA.class "header" ]
        [ H.div [ HA.class "header__title" ] [ Title.view ]
        , H.div [ HA.class "header__score-card" ]
            [ ScoreCard.view reckoning state
                |> H.map onChange
            ]
        ]
