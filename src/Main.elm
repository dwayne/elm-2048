module Main exposing (main)


import App.Data.Tally as Tally exposing (Tally)
import App.View.Main
import App.View.ScoreCard as ScoreCard
import Browser
import Html as H


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
  }


init : () -> (Model, Cmd msg)
init _ =
  ( { tally = Tally.zero
    , scoreCardState = ScoreCard.init
    }
  , Cmd.none
  )


-- UPDATE


type Msg
  = ChangedScoreCard ScoreCard.Msg
  | ClickedNewGame


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
      ( model
      , Cmd.none
      )


-- VIEW


view : Model -> H.Html Msg
view { tally, scoreCardState } =
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
    }
