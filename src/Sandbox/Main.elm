module Sandbox.Main exposing (main)


import App.Data.Points as Points exposing (Points)
import App.View.Score as Score
import App.View.Title as Title
import Browser
import Html as H
import Html.Events as HE
import Random


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Model =
  { points : Points
  , scoreState : Score.State
  }


init : () -> (Model, Cmd msg)
init _ =
  ( { points = Points.zero
    , scoreState = Score.init
    }
  , Cmd.none
  )


-- UPDATE


type Msg
  = ClickedAddPoints
  | GotPoints Points
  | ScoreMsg Score.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickedAddPoints ->
      ( model
      , Random.generate GotPoints pointsGenerator
      )

    GotPoints points ->
      ( { model
        | points = Points.add points model.points
        , scoreState = Score.addDelta points model.scoreState
        }
      , Cmd.none
      )

    ScoreMsg scoreMsg ->
      ( { model | scoreState = Score.update scoreMsg model.scoreState }
      , Cmd.none
      )


pointsGenerator : Random.Generator Points
pointsGenerator =
  Random.uniform 4 [ 8, 12, 16, 20, 24, 28, 32, 36, 40 ]
    |> Random.map Points.fromInt


-- VIEW


view : Model -> H.Html Msg
view model =
  H.div []
    [ H.h1 [] [ H.text "Sandbox" ]
    , viewTitle
    , viewScore model.points model.scoreState
    ]


viewTitle : H.Html msg
viewTitle =
  H.div []
    [ H.h2 [] [ H.text "Title" ]
    , Title.view
    ]


viewScore : Points -> Score.State -> H.Html Msg
viewScore points state =
  H.div []
    [ H.h2 [] [ H.text "Score" ]
    , H.div []
        [ H.p [] [ Score.view "Score" points state |> H.map ScoreMsg ]
        , H.button [ HE.onClick ClickedAddPoints ] [ H.text "Add points" ]
        ]
    , H.p [] [ Score.viewReadOnly "Best" Points.zero ]
    ]
