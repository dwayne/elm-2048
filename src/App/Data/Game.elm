module App.Data.Game exposing
    ( Direction
    , Game
    , Msg
    , Outcome(..)
    , State
    , Status(..)
    , encode
    , getGrid
    , keepPlaying
    , load
    , move
    , new
    , toState
    , update
    )

import App.Data.Grid as Grid exposing (Grid)
import App.Data.Points as Points exposing (Points)
import App.Data.Tally as Tally exposing (Tally)
import Json.Decode as JD
import Json.Encode as JE
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


new : Game -> ( Game, Cmd Msg )
new (Game { tally, grid }) =
    start (Just tally) (Just grid)


load : JE.Value -> ( Game, Cmd Msg )
load value =
    case JD.decodeValue decoder value of
        Ok ((Game { status, tally, grid }) as game) ->
            case status of
                GameOver ->
                    start (Just tally) Nothing

                Playing ->
                    let
                        { current } =
                            Tally.toReckoning tally

                        numTiles =
                            List.length <| Grid.toTiles grid
                    in
                    if Points.isZero current && numTiles == 2 then
                        -- NOTE:
                        -- We do this so that when the user refreshes the page
                        -- it behaves as though they clicked the New Game button.
                        start (Just tally) Nothing

                    else
                        ( game, Cmd.none )

                _ ->
                    ( game, Cmd.none )

        Err _ ->
            start Nothing Nothing


start : Maybe Tally -> Maybe Grid -> ( Game, Cmd Msg )
start maybeTally maybeGrid =
    let
        grid =
            case maybeGrid of
                Nothing ->
                    Grid.empty

                Just grid_ ->
                    Grid.reset grid_
    in
    ( Game
        { status = Playing
        , tally =
            case maybeTally of
                Nothing ->
                    Tally.zero

                Just tally ->
                    Tally.resetCurrent tally
        , grid = grid
        }
    , insertAtMost2Tiles grid
    )


keepPlaying : Game -> Game
keepPlaying ((Game state) as game) =
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


move : Direction -> Game -> ( Outcome, Cmd Msg )
move direction (Game state) =
    case Grid.move direction state.grid of
        Just grid ->
            if state.status == Playing || state.status == KeepPlaying then
                let
                    points =
                        Grid.toPoints grid
                in
                ( if Points.isZero points then
                    NoPoints <|
                        Game { state | grid = grid }

                  else
                    EarnedPoints points <|
                        Game
                            { state
                                | tally = Tally.addPoints points state.tally
                                , grid = grid
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
update msg ((Game state) as game) =
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


encode : Game -> JE.Value
encode (Game { status, tally, grid }) =
    JE.object
        [ ( "status", encodeStatus status )
        , ( "tally", Tally.encode tally )
        , ( "grid", Grid.encode grid )
        ]


encodeStatus : Status -> JE.Value
encodeStatus status =
    case status of
        Playing ->
            JE.string "playing"

        Win ->
            JE.string "win"

        GameOver ->
            JE.string "gameOver"

        KeepPlaying ->
            JE.string "keepPlaying"


decoder : JD.Decoder Game
decoder =
    JD.map Game stateDecoder


stateDecoder : JD.Decoder State
stateDecoder =
    JD.map3 State
        (JD.field "status" statusDecoder)
        (JD.field "tally" Tally.decoder)
        (JD.field "grid" Grid.decoder)


statusDecoder : JD.Decoder Status
statusDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "playing" ->
                        JD.succeed Playing

                    "win" ->
                        JD.succeed Win

                    "gameOver" ->
                        JD.succeed GameOver

                    "keepPlaying" ->
                        JD.succeed KeepPlaying

                    _ ->
                        JD.fail <| "expected one of playing, win, gameOver, or keepPlaying: " ++ s
            )
