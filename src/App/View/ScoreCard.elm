module App.View.ScoreCard exposing
  ( State, init
  , addPoints
  , Msg, update
  , Options, view
  )


import App.Data.Points exposing (Points)
import App.View.Score as Score
import Html as H
import Html.Attributes as HA


type State
  = State Score.State


init : State
init =
  State Score.init


addPoints : Points -> State -> State
addPoints points (State state) =
  State <| Score.addPoints points state


type Msg
  = ChangedScore Score.Msg


update : Msg -> State -> State
update msg (State state) =
  case msg of
    ChangedScore scoreMsg ->
      State <| Score.update scoreMsg state


type alias Options =
  { current : Points
  , best : Points
  }


view : Options -> State -> H.Html Msg
view { current, best } (State state) =
  H.div [ HA.class "score-card" ]
    [ H.div [ HA.class "score-card__score" ]
        [ Score.view "Score" current state
            |> H.map ChangedScore
        ]
    , H.div [ HA.class "score-card__score" ]
        [ Score.viewReadOnly "Best" best ]
    ]
