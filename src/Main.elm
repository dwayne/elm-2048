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
  , gridViewState : Grid.ViewState
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
    , gridViewState = Grid.init <| Grid.toTiles grid
    }
  , generateTiles grid
  )


-- UPDATE


type Msg
  = ChangedScoreCard ScoreCard.Msg
  | ClickedNewGame
  | GeneratedTiles (Bool, Grid)
  | ClickedMoveRight
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
      ( { model | grid = grid, gridViewState = Grid.init <| Grid.toTiles grid }
      , generateTiles grid
      )

    GeneratedTiles (wasSuccessful, grid) ->
      if wasSuccessful then
        ( { model | grid = grid, gridViewState = Grid.init <| Grid.toTiles grid }
        , Cmd.none
        )

      else
        -- TODO: Game over.
        ( model
        , Cmd.none
        )

    ClickedMoveRight ->
      let
        grid =
          Grid.moveRight model.grid
      in
      ( { model | grid = grid, gridViewState = Grid.init <| Grid.toTiles grid }
      , generateTiles grid
      )

    ChangedGrid gridMsg ->
      ( { model | gridViewState = Grid.update gridMsg model.gridViewState }
      , Cmd.none
      )


generateTiles : Grid -> Cmd Msg
generateTiles =
  Random.generate GeneratedTiles << Grid.generator


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.map ChangedGrid Grid.subscriptions


-- VIEW


view : Model -> H.Html Msg
view { tally, scoreCardState, gridViewState } =
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
    , gridViewState = gridViewState
    , onMoveRight = ClickedMoveRight
    }
