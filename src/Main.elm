module Main exposing (main)


import App.Data.Grid as Grid exposing (Grid)
import App.Data.Points as Points
import App.Data.Tally as Tally exposing (Tally)
import App.View.Grid as Grid
import App.View.Main
import App.View.ScoreCard as ScoreCard
import Browser
import Html as H
import Random


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { tally : Tally
  , scoreCardState : ScoreCard.State
  , grid : Grid
  , gridState : Grid.State
  }


init : () -> (Model, Cmd Msg)
init _ =
  let
    grid =
      Grid.empty
  in
  ( { tally = Tally.zero
    , scoreCardState = ScoreCard.init
    , grid = grid
    , gridState = Grid.init grid
    }
  , insertAtMost2Tiles grid
  )


-- UPDATE


type Msg
  = ChangedScoreCard ScoreCard.Msg
  | ClickedNewGame
  | InsertedTiles (Maybe Grid)
  | Moved Grid.Direction
  | ChangedGrid Grid.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangedScoreCard scoreCardMsg ->
      ( { model
        | scoreCardState = ScoreCard.update scoreCardMsg model.scoreCardState
        }
      , Cmd.none
      )

    ClickedNewGame ->
      let
        grid =
          Grid.reset model.grid
      in
      ( { model
        | tally = Tally.resetCurrent model.tally
        , grid = grid
        , gridState = Grid.init grid
        }
      , insertAtMost2Tiles grid
      )

    InsertedTiles maybeGrid ->
      case maybeGrid of
        Just grid ->
          let
            message =
              if Grid.hasMoves grid then
                "There are moves"
              else
                -- Game over
                "No moves"
          in
          ( { model | grid = grid, gridState = Grid.init grid }
          , Cmd.none
          )
          |> Debug.log message

        Nothing ->
          ( model
          , Cmd.none
          )

    Moved direction ->
      case Grid.move direction model.grid of
        Just grid ->
          let
            points =
              Grid.toPoints grid

            model1 =
              { model | grid = grid, gridState = Grid.init grid }

            model2 =
              if Points.isZero points then
                model1
              else
                { model1
                | tally = Tally.addPoints points model1.tally
                , scoreCardState = ScoreCard.addDelta points model1.scoreCardState
                }
          in
          ( model2
          , insertAtMost2Tiles grid
          )

        Nothing ->
          ( model
          , Cmd.none
          )

    ChangedGrid gridMsg ->
      ( { model | gridState = Grid.update gridMsg model.gridState }
      , Cmd.none
      )


insertAtMost2Tiles : Grid -> Cmd Msg
insertAtMost2Tiles =
  Random.generate InsertedTiles << Grid.insertAtMost2Tiles


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.map ChangedGrid Grid.subscriptions


-- VIEW


view : Model -> H.Html Msg
view { tally, scoreCardState, gridState } =
  App.View.Main.view
    { header =
        { current = Tally.getCurrent tally
        , best = Tally.getBest tally
        , scoreCard =
            { state = scoreCardState
            , onChange = ChangedScoreCard
            }
        }
    , gridState = gridState
    , onMove = Moved
    , onNewGame = ClickedNewGame
    }
