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
      ( { model | grid = grid, gridState = Grid.init grid }
      , insertAtMost2Tiles grid
      )

    InsertedTiles maybeGrid ->
      case maybeGrid of
        Just grid ->
          ( { model | grid = grid, gridState = Grid.init grid }
          , Cmd.none
          )

        Nothing ->
          ( model
          , Cmd.none
          )

    Moved direction ->
      case Grid.move direction model.grid of
        Just grid ->
          ( { model | grid = grid, gridState = Grid.init grid }
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
