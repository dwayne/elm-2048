module App.Data.Grid exposing
  ( Grid, empty
  , reset
  , atMost2Tiles
  , toTiles
  , moveRight, moveLeft, moveDown, moveUp
  )


import App.Data.Tile as Tile exposing (Tile)
import App.Data.Tile.Position as Position exposing (Position)
import App.Data.Tile.Value as Value exposing (Value)
import App.Lib.List as List
import Random


type Grid
  = Grid
      { currentId : Int
      , tiles : List Tile
      }


empty : Grid
empty =
  Grid
    { currentId = 0
    , tiles = []
    }


reset : Grid -> Grid
reset (Grid { currentId }) =
  Grid
    { currentId = currentId
    , tiles = []
    }


atMost2Tiles : Grid -> Random.Generator (Maybe Grid)
atMost2Tiles (Grid { currentId, tiles } as grid) =
  let
    unavailablePositions =
      List.map Tile.getPosition tiles
  in
  valueAndPositionGenerator unavailablePositions
    |> Random.andThen
        (\valueAndPositions ->
          if List.isEmpty valueAndPositions then
            Random.constant Nothing

          else
            let
              (newCurrentId, newTiles) =
                addTiles valueAndPositions currentId tiles
            in
            Grid
              { currentId = newCurrentId
              , tiles = newTiles
              }
              |> Just
              |> Random.constant
        )


valueAndPositionGenerator : List Position -> Random.Generator (List (Value, Position))
valueAndPositionGenerator unavailablePositions =
  Position.selectAtMost2 unavailablePositions
    |> Random.andThen
        (\positions ->
          if List.isEmpty positions then
            Random.constant []

          else
            Random.list (List.length positions) Value.twoOrFour
              |> Random.andThen
                  (\values ->
                    Random.constant <| List.zip values positions
                  )
        )


addTiles : List (Value, Position) -> Int -> List Tile -> (Int, List Tile)
addTiles valueAndPositions currentId tiles =
  case valueAndPositions of
    [] ->
      (currentId, tiles)

    (value, position) :: restValueAndPositions ->
      addTiles
        restValueAndPositions
        (currentId + 1)
        (tiles ++ [ Tile.new currentId value position ])


toTiles : Grid -> List Tile
toTiles (Grid { tiles }) =
  tiles


type Direction
  = Right
  | Left
  | Down
  | Up


moveRight : Grid -> Maybe Grid
moveRight =
  move Right


moveLeft : Grid -> Maybe Grid
moveLeft =
  move Left


moveDown : Grid -> Maybe Grid
moveDown =
  move Down


moveUp : Grid -> Maybe Grid
moveUp =
  move Up


type alias Config =
  { nextLastPosition : Position -> Position
  , updateLastPosition : Position -> Position
  , isReadyToVacate : Position -> Position -> Bool
  }


type alias MovementState =
  { currentId : Int
  , lastPosition : Position
  , tileInCell : Maybe Tile.Info
  , newTiles : List Tile
  , atLeastOneTileMoved : Bool
  }


move : Direction -> Grid -> Maybe Grid
move direction (Grid { currentId, tiles })=
  let
    state =
      case direction of
        Right ->
          tiles
            |> age
            |> List.sortWith compareRight
            |> moveHelper
                { nextLastPosition = \{ row } -> Position row 4
                , updateLastPosition = \{ row, col } -> { row = row, col = col - 1 }
                , isReadyToVacate =
                    \tilePosition lastPosition ->
                      tilePosition.row > lastPosition.row
                }
                { currentId = currentId
                , lastPosition = Position 1 4
                , tileInCell = Nothing
                , newTiles = []
                , atLeastOneTileMoved = False
                }

        Left ->
          tiles
            |> age
            |> List.sortWith compareLeft
            |> moveHelper
                { nextLastPosition = \{ row } -> Position row 1
                , updateLastPosition = \{ row, col } -> { row = row, col = col + 1 }
                , isReadyToVacate =
                    \tilePosition lastPosition ->
                      tilePosition.row > lastPosition.row
                }
                { currentId = currentId
                , lastPosition = Position 1 1
                , tileInCell = Nothing
                , newTiles = []
                , atLeastOneTileMoved = False
                }

        Down ->
          tiles
            |> age
            |> List.sortWith compareDown
            |> moveHelper
                { nextLastPosition = \{ col } -> Position 4 col
                , updateLastPosition = \{ row, col } -> { row = row - 1, col = col }
                , isReadyToVacate =
                    \tilePosition lastPosition ->
                      tilePosition.col > lastPosition.col
                }
                { currentId = currentId
                , lastPosition = Position 4 1
                , tileInCell = Nothing
                , newTiles = []
                , atLeastOneTileMoved = False
                }

        Up ->
          tiles
            |> age
            |> List.sortWith compareUp
            |> moveHelper
                { nextLastPosition = \{ col } -> Position 1 col
                , updateLastPosition = \{ row, col } -> { row = row + 1, col = col }
                , isReadyToVacate =
                    \tilePosition lastPosition ->
                      tilePosition.col > lastPosition.col
                }
                { currentId = currentId
                , lastPosition = Position 1 1
                , tileInCell = Nothing
                , newTiles = []
                , atLeastOneTileMoved = False
                }
  in
  if state.atLeastOneTileMoved then
    Just <|
      Grid
        { currentId = state.currentId
        , tiles = state.newTiles
        }
  else
    Nothing


moveHelper : Config -> MovementState -> List Tile -> MovementState
moveHelper config state tiles =
  case tiles of
    [] ->
      case state.tileInCell of
        Nothing ->
          state

        Just { id, value, from, to } ->
          { state
          | lastPosition = to
          , tileInCell = Nothing
          , newTiles =
              state.newTiles ++ [ Tile.old id value from to ]
          }

    tile :: restTiles ->
      let
        currTile =
          Tile.toInfo tile

        state1 =
          if config.isReadyToVacate currTile.to state.lastPosition then
            { state
            | lastPosition = config.nextLastPosition currTile.to
            , tileInCell = Nothing
            , newTiles =
                (++) state.newTiles <|
                  case state.tileInCell of
                    Nothing ->
                      []

                    Just { id, value, from, to } ->
                      [ Tile.old id value from to ]
            }
          else
            state

        state2 =
          case state1.tileInCell of
            Nothing ->
              { state1
              | tileInCell =
                  Just { currTile | from = currTile.to, to = state1.lastPosition }
              , atLeastOneTileMoved =
                  state1.atLeastOneTileMoved || (currTile.to /= state1.lastPosition)
              }

            Just { id, value, from, to } ->
              let
                lastPosition =
                  config.updateLastPosition state1.lastPosition
              in
              if Value.isEqual currTile.value value then
                { state1
                | newTiles =
                    (++) state1.newTiles <|
                      [ Tile.merged currTile.id currTile.value currTile.to to
                      , Tile.merged id value from to
                      , Tile.composite
                          state1.currentId
                          (Value.double value)
                          to
                      ]
                , currentId = state1.currentId + 1
                , tileInCell = Nothing
                , lastPosition = lastPosition
                , atLeastOneTileMoved = True
                }
              else
                { state1
                | newTiles =
                    state1.newTiles ++ [ Tile.old id value from to ]
                , tileInCell =
                    Just { currTile | from = currTile.to, to = lastPosition }
                , lastPosition = lastPosition
                , atLeastOneTileMoved =
                    state1.atLeastOneTileMoved || (currTile.to /= lastPosition)
                }
      in
      moveHelper config state2 restTiles


age : List Tile -> List Tile
age =
  List.filterMap Tile.age


compareRight : Tile -> Tile -> Order
compareRight tile1 tile2 =
  let
    p1 =
      Tile.getPosition tile1

    p2 =
      Tile.getPosition tile2
  in
  if p1.row < p2.row then
    LT
  else if p1.row > p2.row then
    GT
  else
    compare p2.col p1.col


compareLeft : Tile -> Tile -> Order
compareLeft tile1 tile2 =
  let
    p1 =
      Tile.getPosition tile1

    p2 =
      Tile.getPosition tile2
  in
  if p1.row < p2.row then
    LT
  else if p1.row > p2.row then
    GT
  else
    compare p1.col p2.col


compareDown : Tile -> Tile -> Order
compareDown tile1 tile2 =
  let
    p1 =
      Tile.getPosition tile1

    p2 =
      Tile.getPosition tile2
  in
  if p1.col < p2.col then
    LT
  else if p1.col > p2.col then
    GT
  else
    compare p2.row p1.row


compareUp : Tile -> Tile -> Order
compareUp tile1 tile2 =
  let
    p1 =
      Tile.getPosition tile1

    p2 =
      Tile.getPosition tile2
  in
  if p1.col < p2.col then
    LT
  else if p1.col > p2.col then
    GT
  else
    compare p1.row p2.row
