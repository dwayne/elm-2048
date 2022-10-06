module App.View.Score exposing
  ( State, init, addPoints
  , Msg, update
  , viewCurrent, viewBest
  )


import App.Data.Points as Points exposing (Points)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Json.Decode as JD


-- STATE


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


addPoints : Points -> State -> State
addPoints points (State state) =
  State
    { state
    | currentId = state.currentId + 1
    , deltas = state.deltas ++ [(state.currentId, points)]
    }


-- UPDATE


type Msg
  = AnimationEnded


update : Msg -> State -> State
update msg (State state) =
  case msg of
    AnimationEnded ->
      State
        { state
        | deltas = List.tail state.deltas |> Maybe.withDefault []
        }


-- VIEW


viewCurrent : Points -> State -> H.Html Msg
viewCurrent =
  view "Score"


viewBest : Points -> H.Html msg
viewBest =
  viewReadOnly "Best"


view : String -> Points -> State -> H.Html Msg
view title points (State { deltas }) =
  let
    (pointsAsString, scoreNDigit) =
      pointsToDetails points
  in
  H.div [ HA.class "score", scoreNDigit ]
    [ H.h2 [ HA.class "score__title" ] [ H.text title ]
    , let
        scoreValue =
          ( "score__value"
          , H.div [ HA.class "score__value" ] [ H.text pointsAsString ]
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
  let
    (pointsAsString, scoreNDigit) =
      pointsToDetails points
  in
  H.div [ HA.class "score", scoreNDigit ]
    [ H.h2 [ HA.class "score__title" ] [ H.text title ]
    , H.div [ HA.class "score__total" ]
        [ H.div [ HA.class "score__value" ] [ H.text pointsAsString ] ]
    ]


pointsToDetails : Points -> (String, H.Attribute msg)
pointsToDetails points =
  let
    pointsAsString =
      Points.toString points

    digits =
      String.length pointsAsString
  in
  ( pointsAsString
  , HA.classList
      [ ( "score--" ++ String.fromInt digits ++ "-digit"
        , digits > 3
        )
      ]
  )
