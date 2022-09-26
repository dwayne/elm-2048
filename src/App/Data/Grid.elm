module App.Data.Grid exposing
  ( Grid, empty
  , reset
  , atMost2Tiles
  , toTiles
  , moveRight, moveLeft
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


type alias MovementState =
  { currentId : Int
  , lastPosition : Position
  , tileInCell : Maybe Tile.Info
  , newTiles : List Tile
  , atLeastOneTileMoved : Bool
  }


moveRight : Grid -> Maybe Grid
moveRight (Grid { currentId, tiles }) =
  let
    state =
      tiles
        |> age
        |> sortRight
        |> moveRightHelper
            { currentId = currentId
            , lastPosition = Position 1 4
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


sortRight : List Tile -> List Tile
sortRight =
  List.sortWith compareRight


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


moveRightHelper : MovementState -> List Tile -> MovementState
moveRightHelper state tiles =
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
          if currTile.to.row > state.lastPosition.row then
            { state
            | lastPosition = Position currTile.to.row 4
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
                  Position
                    state1.lastPosition.row
                    (state1.lastPosition.col - 1)
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
      moveRightHelper state2 restTiles


moveLeft : Grid -> Maybe Grid
moveLeft (Grid { currentId, tiles }) =
  let
    state =
      tiles
        |> age
        |> sortLeft
        |> moveLeftHelper
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


sortLeft : List Tile -> List Tile
sortLeft =
  List.sortWith compareLeft


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


moveLeftHelper : MovementState -> List Tile -> MovementState
moveLeftHelper state tiles =
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
          if currTile.to.row > state.lastPosition.row then
            { state
            | lastPosition = Position currTile.to.row 1
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
                  Position
                    state1.lastPosition.row
                    (state1.lastPosition.col + 1)
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
      moveLeftHelper state2 restTiles


age : List Tile -> List Tile
age =
  List.filterMap Tile.age
