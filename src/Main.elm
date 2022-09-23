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
  , currentId : Int
  , grid : Grid
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { tally = Tally.zero
    , scoreCardState = ScoreCard.init
    , currentId = 0
    , grid = Grid.empty
    }
  , generateTiles Grid.empty
  )


-- UPDATE


type Msg
  = ChangedScoreCard ScoreCard.Msg
  | ClickedNewGame
  | GeneratedTiles (Maybe Grid)


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
      ( { model | currentId = model.currentId + 2, grid = Grid.empty }
      , generateTiles Grid.empty
      )

    GeneratedTiles maybeGrid ->
      case maybeGrid of
        Nothing ->
          -- TODO: Game over.
          ( model
          , Cmd.none
          )

        Just grid ->
          ( { model | grid = grid }
          , Cmd.none
          )


generateTiles : Grid -> Cmd Msg
generateTiles =
  Random.generate GeneratedTiles << Grid.generator


-- VIEW


view : Model -> H.Html Msg
view { tally, scoreCardState, currentId, grid } =
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
    , currentId = currentId
    , grid = grid
    }
