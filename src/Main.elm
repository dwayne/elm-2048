module Main exposing (main)


import App.Data.Grid as Grid exposing (Grid)
import App.Data.Tally as Tally exposing (Tally)
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
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Model =
  { tally : Tally
  , scoreCardState : ScoreCard.State
  , grid : Grid
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
    }
  , generateTiles grid
  )


-- UPDATE


type Msg
  = ChangedScoreCard ScoreCard.Msg
  | ClickedNewGame
  | GeneratedTiles (Bool, Grid)


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
      ( { model | grid = grid }
      , generateTiles grid
      )

    GeneratedTiles (wasSuccessful, grid) ->
      if wasSuccessful then
        ( { model | grid = grid }
        , Cmd.none
        )

      else
        -- TODO: Game over.
        ( model
        , Cmd.none
        )


generateTiles : Grid -> Cmd Msg
generateTiles =
  Random.generate GeneratedTiles << Grid.generator


-- VIEW


view : Model -> H.Html Msg
view { tally, scoreCardState, grid } =
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
    , grid = grid
    }
