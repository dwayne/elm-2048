module App.View.Score exposing
  ( State, init
  , addDelta

  , Msg, update

  , view, viewReadOnly
  )


import App.Data.Points as Points exposing (Points)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Json.Decode as JD


type State
  = State
      { currentId : Int
      , deltas : List (Int, Points)
      }


init : State
init =
  State
    { currentId = 0
    , deltas = []
    }


addDelta : Points -> State -> State
addDelta points (State state) =
  State
    { state
    | currentId = state.currentId + 1
    , deltas = state.deltas ++ [(state.currentId, points)]
    }


type Msg
  = AnimationEnded


update : Msg -> State -> State
update msg (State state) =
  case msg of
    AnimationEnded ->
      State { state | deltas = Maybe.withDefault [] <| List.tail state.deltas }


view : String -> Points -> State -> H.Html Msg
view title points (State { deltas }) =
  H.div [ HA.class "score" ]
    [ H.h2 [ HA.class "score__title" ] [ H.text title ]
    , let
        scoreValue =
          ( "score__value"
          , H.div [ HA.class "score__value" ]
              [ H.text <| Points.toString points ]
          )

        scoreDeltas =
          List.map viewScoreDelta deltas
      in
      HK.node "div" [ HA.class "score__total" ] <|
        scoreValue :: scoreDeltas
    ]


viewScoreDelta : (Int, Points) -> (String, H.Html Msg)
viewScoreDelta (id, points) =
  ( String.fromInt id
  , H.div
      [ HA.class "score__delta"
      , onAnimationEnd AnimationEnded
      ]
      [ H.text <| "+" ++ Points.toString points ]
  )


onAnimationEnd : msg -> H.Attribute msg
onAnimationEnd msg =
  HE.on "animationend" <| JD.succeed msg


viewReadOnly : String -> Points -> H.Html msg
viewReadOnly title points =
  H.div [ HA.class "score" ]
    [ H.h2 [ HA.class "score__title" ] [ H.text title ]
    , H.div [ HA.class "score__total" ]
        [ H.div [ HA.class "score__value" ]
            [ H.text <| Points.toString points ]
        ]
    ]