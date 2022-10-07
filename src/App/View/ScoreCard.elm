module App.View.ScoreCard exposing
  ( State, init, addPoints
  , Msg, update
  , view
  )


import App.Data.Points exposing (Points)
import App.Data.Tally exposing (Reckoning)
import App.View.Score as Score
import Html as H
import Html.Attributes as HA


-- STATE


type State
  = State Score.State


init : State
init =
  State Score.init


addPoints : Points -> State -> State
addPoints points (State state) =
  State <| Score.addPoints points state


-- UPDATE


type Msg
  = ChangedCurrentScore Score.Msg


update : Msg -> State -> State
update msg (State state) =
  case msg of
    ChangedCurrentScore scoreMsg ->
      State <| Score.update scoreMsg state


-- VIEW


view : Reckoning -> State -> H.Html Msg
view { current, best } (State state) =
  H.div [ HA.class "score-card" ]
    [ H.div [ HA.class "score-card__score" ]
        [ Score.viewCurrent current state
            |> H.map ChangedCurrentScore
        ]
    , H.div [ HA.class "score-card__score" ] [ Score.viewBest best ]
    ]
