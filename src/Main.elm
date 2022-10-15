module Main exposing (main)

import App.Data.Game as Game exposing (Game)
import App.Data.Tally as Tally
import App.Port as Port
import App.View.Grid as Grid
import App.View.Main as MainView
import App.View.ScoreCard as ScoreCard
import Browser
import Browser.Dom as BD
import Html as H
import Json.Encode as JE
import Random
import Task


main : Program JE.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- CONSTANTS


htmlIds =
    { mainView = "main"
    , tryAgain = "tryAgain"
    , keepPlaying = "keepPlaying"
    }



-- MODEL


type alias Model =
    { game : Game
    , scoreCardState : ScoreCard.State
    , gridState : Grid.State
    , mainViewState : MainView.State
    }


init : JE.Value -> ( Model, Cmd Msg )
init value =
    let
        ( game, cmd ) =
            Game.load value
    in
    ( { game = game
      , scoreCardState = ScoreCard.init
      , gridState = toGridState game
      , mainViewState = MainView.init
      }
    , Cmd.batch
        [ focus htmlIds.mainView
        , Cmd.map ChangedGame cmd
        ]
    )



-- UPDATE


type Msg
    = Focused
    | ClickedNewGame
    | ClickedKeepPlaying
    | Moved Game.Direction
    | OpenedWinMessage
    | OpenedGameOverMessage
    | ChangedGame Game.Msg
    | ChangedScoreCard ScoreCard.Msg
    | ChangedGrid Grid.Msg
    | ChangedMainView MainView.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focused ->
            ( model
            , Cmd.none
            )

        ClickedNewGame ->
            let
                ( game, cmd ) =
                    Game.new model.game
            in
            ( { model | game = game, gridState = toGridState game }
            , Cmd.batch
                [ focus htmlIds.mainView
                , Port.save game
                , Cmd.map ChangedGame cmd
                ]
            )

        ClickedKeepPlaying ->
            let
                game =
                    Game.keepPlaying model.game
            in
            ( { model | game = game }
            , Cmd.batch
                [ focus htmlIds.mainView
                , Port.save game
                ]
            )

        Moved direction ->
            let
                ( outcome, cmd ) =
                    Game.move direction model.game
            in
            case outcome of
                Game.NoMovement ->
                    ( model
                    , Cmd.none
                    )

                Game.NoPoints game ->
                    ( { model | game = game, gridState = toGridState game }
                    , Cmd.batch
                        [ Port.save game
                        , Cmd.map ChangedGame cmd
                        ]
                    )

                Game.EarnedPoints points game ->
                    ( { model
                        | game = game
                        , scoreCardState = ScoreCard.addPoints points model.scoreCardState
                        , gridState = toGridState game
                      }
                    , Cmd.batch
                        [ Port.save game
                        , Cmd.map ChangedGame cmd
                        ]
                    )

        OpenedWinMessage ->
            ( model
            , focus htmlIds.keepPlaying
            )

        OpenedGameOverMessage ->
            ( model
            , focus htmlIds.tryAgain
            )

        ChangedGame gameMsg ->
            let
                game =
                    Game.update gameMsg model.game
            in
            ( { model | game = game, gridState = toGridState game }
            , Port.save game
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
                ( mainViewState, cmd ) =
                    MainView.update { onMove = Moved } mainMsg model.mainViewState
            in
            ( { model | mainViewState = mainViewState }
            , cmd
            )


focus : String -> Cmd Msg
focus =
    Task.attempt (always Focused) << BD.focus


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
        { id = htmlIds.mainView
        , header =
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
                        { id = htmlIds.keepPlaying
                        , onKeepPlaying = ClickedKeepPlaying
                        , onTryAgain = ClickedNewGame
                        , onOpen = OpenedWinMessage
                        }

                Game.GameOver ->
                    Grid.GameOverMessage
                        { id = htmlIds.tryAgain
                        , onTryAgain = ClickedNewGame
                        , onOpen = OpenedGameOverMessage
                        }

                Game.KeepPlaying ->
                    Grid.NoMessage
        , gridState = gridState
        , onMove = Moved
        , onNewGame = ClickedNewGame
        , onChange = ChangedMainView
        }
