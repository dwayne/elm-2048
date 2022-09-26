module Main exposing (main)


import App.Data.Grid as Grid exposing (Grid)
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
  , generateAtMost2Tiles grid
  )


-- UPDATE


type Msg
  = ChangedScoreCard ScoreCard.Msg
  | ClickedNewGame
  | GeneratedTiles (Maybe Grid)
  | ClickedMoveRight
  | ClickedMoveLeft
  | ClickedMoveDown
  | ClickedMoveUp
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
      ( { model | grid = grid, gridState = Grid.init grid }
      , generateAtMost2Tiles grid
      )

    GeneratedTiles maybeGrid ->
      case maybeGrid of
        Just grid ->
          ( { model | grid = grid, gridState = Grid.init grid }
          , Cmd.none
          )

        Nothing ->
          ( model
          , Cmd.none
          )

    ClickedMoveRight ->
      case Grid.moveRight model.grid of
        Just grid ->
          ( { model | grid = grid, gridState = Grid.init grid }
          , generateAtMost2Tiles grid
          )

        Nothing ->
          ( model
          , Cmd.none
          )

    ClickedMoveLeft ->
      case Grid.moveLeft model.grid of
        Just grid ->
          ( { model | grid = grid, gridState = Grid.init grid }
          , generateAtMost2Tiles grid
          )

        Nothing ->
          ( model
          , Cmd.none
          )

    ClickedMoveDown ->
      case Grid.moveDown model.grid of
        Just grid ->
          ( { model | grid = grid, gridState = Grid.init grid }
          , generateAtMost2Tiles grid
          )

        Nothing ->
          ( model
          , Cmd.none
          )

    ClickedMoveUp ->
      case Grid.moveUp model.grid of
        Just grid ->
          ( { model | grid = grid, gridState = Grid.init grid }
          , generateAtMost2Tiles grid
          )

        Nothing ->
          ( model
          , Cmd.none
          )

    ChangedGrid gridMsg ->
      ( { model | gridState = Grid.update gridMsg model.gridState }
      , Cmd.none
      )


generateAtMost2Tiles : Grid -> Cmd Msg
generateAtMost2Tiles =
  Random.generate GeneratedTiles << Grid.atMost2Tiles


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
    , onNewGame = ClickedNewGame
    , gridState = gridState
    , onMoveRight = ClickedMoveRight
    , onMoveLeft = ClickedMoveLeft
    , onMoveDown = ClickedMoveDown
    , onMoveUp = ClickedMoveUp
    }
