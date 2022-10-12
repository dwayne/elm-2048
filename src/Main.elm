module Main exposing (main)


import App.Data.Game as Game exposing (Game)
import App.Data.Tally as Tally
import App.View.Grid as Grid
import App.View.Main as MainView
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
  { game : Game
  , scoreCardState : ScoreCard.State
  , gridState : Grid.State
  , mainViewState : MainView.State
  }


init : () -> (Model, Cmd Msg)
init _ =
  let
    (game, cmd) =
      Game.start
  in
  ( { game = game
    , scoreCardState = ScoreCard.init
    , gridState = toGridState game
    , mainViewState = MainView.init
    }
  , Cmd.map ChangedGame cmd
  )


-- UPDATE


type Msg
  = ClickedNewGame
  | ClickedKeepPlaying
  | Moved Game.Direction
  | ChangedGame Game.Msg
  | ChangedScoreCard ScoreCard.Msg
  | ChangedGrid Grid.Msg
  | ChangedMainView MainView.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickedNewGame ->
      let
        (game, cmd) =
          Game.new model.game
      in
      ( { model | game = game, gridState = toGridState game }
      , Cmd.map ChangedGame cmd
      )

    ClickedKeepPlaying ->
      ( { model | game = Game.keepPlaying model.game }
      , Cmd.none
      )

    Moved direction ->
      let
        (outcome, cmd) =
          Game.move direction model.game
      in
      case outcome of
        Game.NoMovement ->
          ( model
          , Cmd.none
          )

        Game.NoPoints game ->
          ( { model | game = game, gridState = toGridState game }
          , Cmd.map ChangedGame cmd
          )

        Game.EarnedPoints points game ->
          ( { model
            | game = game
            , scoreCardState = ScoreCard.addPoints points model.scoreCardState
            , gridState = toGridState game
            }
          , Cmd.map ChangedGame cmd
          )

    ChangedGame gameMsg ->
      let
        game =
          Game.update gameMsg model.game
      in
      ( { model | game = game, gridState = toGridState game }
      , Cmd.none
      )

    ChangedScoreCard scoreCardMsg ->
      ( { model
        | scoreCardState = ScoreCard.update scoreCardMsg model.scoreCardState
        }
      , Cmd.none
      )

    ChangedGrid gridMsg ->
      ( { model | gridState = Grid.update gridMsg model.gridState }
      , Cmd.none
      )

    ChangedMainView mainMsg ->
      let
        (mainViewState, cmd) =
          MainView.update { onMove = Moved } mainMsg model.mainViewState
      in
      ( { model | mainViewState = mainViewState }
      , cmd
      )


toGridState : Game -> Grid.State
toGridState =
  Grid.fromGrid << Game.getGrid


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.map ChangedGrid Grid.subscriptions


-- VIEW


view : Model -> H.Html Msg
view { game, scoreCardState, gridState } =
  let
    { status, tally } =
      Game.toState game
  in
  MainView.view
    { header =
        { reckoning = Tally.toReckoning tally
        , state = scoreCardState
        , onChange = ChangedScoreCard
        }
    , message =
        case status of
          Game.Playing ->
            Grid.NoMessage

          Game.Win ->
            Grid.WinMessage
              { onKeepPlaying = ClickedKeepPlaying
              , onTryAgain = ClickedNewGame
              }

          Game.GameOver ->
            Grid.GameOverMessage
              { onTryAgain = ClickedNewGame
              }

          Game.KeepPlaying ->
            Grid.NoMessage
    , gridState = gridState
    , onMove = Moved
    , onNewGame = ClickedNewGame
    , onChange = ChangedMainView
    }
