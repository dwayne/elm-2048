module App.Data.Game exposing
  ( Game, Status(..)
  , start, new
  , keepPlaying
  , getGrid
  , Direction, Outcome(..), move
  , Msg, update
  , State, toState
  )


import App.Data.Grid as Grid exposing (Grid)
import App.Data.Points as Points exposing (Points)
import App.Data.Tally as Tally exposing (Tally)
import Random


type Game
  = Game State

type alias State =
  { status : Status
  , tally : Tally
  , grid : Grid
  }

type Status
  = Playing
  | Win
  | GameOver
  | KeepPlaying


type Msg
  = InsertedTiles (Maybe Grid)


start : (Game, Cmd Msg)
start =
  let
    grid =
      Grid.empty
  in
  ( Game
      { status = Playing
      , tally = Tally.zero
      , grid = grid
      }
  , insertAtMost2Tiles grid
  )


new : Game -> (Game, Cmd Msg)
new (Game state) =
  let
    grid =
      Grid.reset state.grid
  in
  ( Game
      { status = Playing
      , tally = Tally.resetCurrent state.tally
      , grid = grid
      }
  , insertAtMost2Tiles grid
  )


keepPlaying : Game -> Game
keepPlaying (Game state as game) =
  if state.status == Win then
    Game { state | status = KeepPlaying }
  else
    game


getGrid : Game -> Grid
getGrid (Game { grid }) =
  grid


type alias Direction =
  Grid.Direction


type Outcome
  = NoMovement
  | NoPoints Game
  | EarnedPoints Points Game


move : Direction -> Game -> (Outcome, Cmd Msg)
move direction (Game state as game) =
  case Grid.move direction state.grid of
    Just grid ->
      let
        points =
          Grid.toPoints grid
      in
      if state.status == Playing || state.status == KeepPlaying then
        ( if Points.isZero points then
            NoPoints <|
              Game { state | grid = grid }

          else
            EarnedPoints points <|
              Game
                { state
                | grid = grid
                , tally = Tally.addPoints points state.tally
                }
        , insertAtMost2Tiles grid
        )
      else
        ( NoMovement
        , Cmd.none
        )

    Nothing ->
      ( NoMovement
      , Cmd.none
      )


insertAtMost2Tiles : Grid -> Cmd Msg
insertAtMost2Tiles =
  Random.generate InsertedTiles << Grid.insertAtMost2Tiles


update : Msg -> Game -> Game
update msg (Game state as game) =
  case msg of
    InsertedTiles maybeGrid ->
      case maybeGrid of
        Just grid ->
          case state.status of
            Playing ->
              if Grid.has2048 grid then
                Game { state | grid = grid, status = Win }

              else if Grid.hasMoves grid then
                Game { state | grid = grid }

              else
                Game { state | grid = grid, status = GameOver }

            KeepPlaying ->
              if Grid.hasMoves grid then
                Game { state | grid = grid }
              else
                Game { state | grid = grid, status = GameOver }

            _ ->
              game

        Nothing ->
          game


toState : Game -> State
toState (Game state) =
  state
