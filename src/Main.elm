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
  { status : Status
  , tally : Tally
  , scoreCardState : ScoreCard.State
  , grid : Grid
  , gridState : Grid.State
  }


type Status
  = Playing
  | Won
  | Loss
  | KeepPlaying


init : () -> (Model, Cmd Msg)
init _ =
  let
    grid =
      Grid.empty
  in
  ( { status = Playing
    , tally = Tally.zero
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
  | ClickedKeepPlaying
  | InsertedTiles (Maybe Grid)
  | Moved Grid.Direction
  | ChangedGrid Grid.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.status of
    Playing ->
      updatePlaying msg model

    Won ->
      updateGameOver msg model

    Loss ->
      updateGameOver msg model

    KeepPlaying ->
      updateKeepPlaying msg model


updatePlaying : Msg -> Model -> (Model, Cmd Msg)
updatePlaying msg model =
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

    ClickedKeepPlaying ->
      ( model
      , Cmd.none
      )

    InsertedTiles maybeGrid ->
      case maybeGrid of
        Just grid ->
          if Grid.has2048 grid then
            ( { model | status = Won, grid = grid, gridState = Grid.init grid }
            , Cmd.none
            )
          else if Grid.hasMoves grid then
            ( { model | grid = grid, gridState = Grid.init grid }
            , Cmd.none
            )
          else
            ( { model | status = Loss, grid = grid, gridState = Grid.init grid }
            , Cmd.none
            )

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


updateGameOver : Msg -> Model -> (Model, Cmd Msg)
updateGameOver msg model =
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
        | status = Playing
        , tally = Tally.resetCurrent model.tally
        , grid = grid
        , gridState = Grid.init grid
        }
      , insertAtMost2Tiles grid
      )

    ClickedKeepPlaying ->
      ( { model | status = KeepPlaying }
      , Cmd.none
      )

    InsertedTiles _ ->
      ( model
      , Cmd.none
      )

    Moved _ ->
      ( model
      , Cmd.none
      )

    ChangedGrid gridMsg ->
      ( { model | gridState = Grid.update gridMsg model.gridState }
      , Cmd.none
      )


updateKeepPlaying : Msg -> Model -> (Model, Cmd Msg)
updateKeepPlaying msg model =
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
        | status = Playing
        , tally = Tally.resetCurrent model.tally
        , grid = grid
        , gridState = Grid.init grid
        }
      , insertAtMost2Tiles grid
      )

    ClickedKeepPlaying ->
      ( model
      , Cmd.none
      )

    InsertedTiles maybeGrid ->
      case maybeGrid of
        Just grid ->
          if Grid.hasMoves grid then
            ( { model | grid = grid, gridState = Grid.init grid }
            , Cmd.none
            )
          else
            ( { model | status = Loss, grid = grid, gridState = Grid.init grid }
            , Cmd.none
            )

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
view { status, tally, scoreCardState, gridState } =
  App.View.Main.view
    { header =
        { current = Tally.getCurrent tally
        , best = Tally.getBest tally
        , scoreCard =
            { state = scoreCardState
            , onChange = ChangedScoreCard
            }
        }
    , message =
        case status of
          Playing ->
            Grid.NoMessage

          Won ->
            Grid.WinMessage
              { onTryAgain = ClickedNewGame
              , onKeepPlaying = ClickedKeepPlaying
              }

          Loss ->
            Grid.GameOverMessage
              { onTryAgain = ClickedNewGame
              }

          KeepPlaying ->
            Grid.NoMessage
    , gridState = gridState
    , onMove = Moved
    , onNewGame = ClickedNewGame
    }
