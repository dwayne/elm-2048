module Sandbox.Main exposing (main)


import App.Data.Grid as Grid
import App.Data.Points as Points exposing (Points)
import App.Data.Tally as Tally exposing (Tally)
import App.Data.Tile.Value as Value
import App.View.Footer as Footer
import App.View.Grid as Grid
import App.View.Header as Header
import App.View.Introduction as Introduction
import App.View.Score as Score
import App.View.ScoreCard as ScoreCard
import App.View.Title as Title
import Browser
import Html as H
import Html.Attributes as HA
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
  , tally : Tally
  , scoreCardState : ScoreCard.State
  }


init : () -> (Model, Cmd msg)
init _ =
  ( { points = Points.zero
    , scoreState = Score.init
    , tally = Tally.zero
    , scoreCardState = ScoreCard.init
    }
  , Cmd.none
  )


-- UPDATE


type Msg
  -- Score
  = ClickedAddPoints1
  | GotPoints1 Points
  | ChangedScore Score.Msg

  -- ScoreCard
  | ClickedReset
  | ClickedAddPoints2
  | GotPoints2 Points
  | ChangedScoreCard ScoreCard.Msg

  -- Introduction
  | ClickedNewGame


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickedAddPoints1 ->
      ( model
      , Random.generate GotPoints1 pointsGenerator
      )

    GotPoints1 points ->
      ( { model
        | points = Points.add points model.points
        , scoreState = Score.addPoints points model.scoreState
        }
      , Cmd.none
      )

    ChangedScore scoreMsg ->
      ( { model | scoreState = Score.update scoreMsg model.scoreState }
      , Cmd.none
      )

    ClickedReset ->
      ( { model | tally = Tally.resetCurrent model.tally }
      , Cmd.none
      )

    ClickedAddPoints2 ->
      ( model
      , Random.generate GotPoints2 pointsGenerator
      )

    GotPoints2 points ->
      ( { model
        | tally = Tally.addPoints points model.tally
        , scoreCardState = ScoreCard.addPoints points model.scoreCardState
        }
      , Cmd.none
      )

    ChangedScoreCard scoreCardMsg ->
      ( { model
        | scoreCardState = ScoreCard.update scoreCardMsg model.scoreCardState
        }
      , Cmd.none
      )

    ClickedNewGame ->
      ( { model | tally = Tally.resetCurrent model.tally }
      , Cmd.none
      )


pointsGenerator : Random.Generator Points
pointsGenerator =
  let
    four =
      Points.fromValue Value.four

    makeList n accum list =
      if n < 1 then
        list
      else
        let
          newAccum =
            Points.add accum four
        in
        makeList (n - 1) newAccum (list ++ [ newAccum ])

  in
  makeList 10 four []
    |> Random.uniform four



-- VIEW


view : Model -> H.Html Msg
view model =
  H.div []
    [ H.h1 [] [ H.text "Sandbox" ]
    -- , viewTitle
    -- , viewScore model.points model.scoreState
    -- , viewScoreCard model.tally model.scoreCardState
    , viewHeader model.tally model.scoreCardState
    , viewIntroduction
    , viewGrid
    , viewFooter
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
        [ H.p [] [ Score.view "Score" points state |> H.map ChangedScore ]
        , H.button [ HE.onClick ClickedAddPoints1 ] [ H.text "Add points" ]
        ]
    , H.p [] [ Score.viewReadOnly "Best" Points.zero ]
    ]


viewScoreCard : Tally -> ScoreCard.State -> H.Html Msg
viewScoreCard tally state =
  H.div []
    [ H.h2 [] [ H.text "Score Card" ]
    , ScoreCard.view
        { current = Tally.getCurrent tally
        , best = Tally.getBest tally
        }
        state
          |> H.map ChangedScoreCard
    , H.p []
        [ H.button [ HE.onClick ClickedReset ] [ H.text "Reset" ]
        , H.text " "
        , H.button [ HE.onClick ClickedAddPoints2 ] [ H.text "Add points" ]
        ]
    ]


viewHeader : Tally -> ScoreCard.State -> H.Html Msg
viewHeader tally state =
  H.div []
    [ H.h2 [] [ H.text "Header" ]
    , H.div
        [ HA.style "min-width" "280px"
        , HA.style "max-width" "500px"
        ]
        [ Header.view
            { current = Tally.getCurrent tally
            , best = Tally.getBest tally
            , scoreCard =
                { state = state
                , onChange = ChangedScoreCard
                }
            }
        ]
    , H.p []
        -- [ H.button [ HE.onClick ClickedReset ] [ H.text "Reset" ]
        -- , H.text " "
        -- , H.button [ HE.onClick ClickedAddPoints2 ] [ H.text "Add points" ]
        -- ]
        [ H.button [ HE.onClick ClickedAddPoints2 ] [ H.text "Add points" ] ]
    ]


viewIntroduction : H.Html Msg
viewIntroduction =
  H.div []
    [ H.h2 [] [ H.text "Introduction" ]
    , H.div
        [ HA.style "min-width" "280px"
        , HA.style "max-width" "500px"
        ]
        [ Introduction.view ClickedNewGame ]
    ]


viewGrid : H.Html msg
viewGrid =
  H.div []
    [ H.h2 [] [ H.text "Grid" ]
    , Grid.view Grid.NoMessage <| Grid.init Grid.empty
    ]


viewFooter : H.Html Msg
viewFooter =
  H.div []
    [ H.h2 [] [ H.text "Footer" ]
    , H.div
        [ HA.style "min-width" "280px"
        , HA.style "max-width" "500px"
        ]
        [ Footer.view ]
    ]
