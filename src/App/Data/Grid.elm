module App.Data.Grid exposing
  ( Grid, empty
  , reset
  , generator
  , toTiles

  , moveRight

  , unsafeFromTiles
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


generator : Grid -> Random.Generator (Bool, Grid)
generator (Grid { currentId, tiles } as grid) =
  getUnavailablePositions tiles
    |> valueAndPositionGenerator
    |> Random.andThen
        (\valueAndPositions ->
          if List.isEmpty valueAndPositions then
            Random.constant (False, grid)

          else
            let
              (newCurrentId, newTiles) =
                addTiles valueAndPositions currentId tiles
            in
            Random.constant
              ( True
              , Grid
                  { currentId = newCurrentId
                  , tiles = newTiles
                  }
              )
        )


valueAndPositionGenerator : List Position -> Random.Generator (List (Value, Position))
valueAndPositionGenerator unavailablePositions =
  Position.generator unavailablePositions
    |> Random.andThen
        (\positions ->
          if List.isEmpty positions then
            Random.constant []

          else
            Random.list (List.length positions) Value.generator
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


getUnavailablePositions : List Tile -> List Position
getUnavailablePositions =
  List.map Tile.getPosition


toTiles : Grid -> List Tile
toTiles (Grid { tiles }) =
  List.sortWith Tile.comparator tiles


-- MOVE


type alias MovementState =
  { currentId : Int
  , lastPosition : Position
  , tileInCell : Maybe Tile.State
  , newTiles : List Tile
  }


moveRight : Grid -> Grid
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
            }
  in
  Grid
    { currentId = state.currentId
    , tiles = state.newTiles
    }


sortRight : List Tile.State -> List Tile.State
sortRight =
  List.sortWith compareRight


compareRight : Tile.State -> Tile.State -> Order
compareRight tile1 tile2 =
  let
    p1 =
      tile1.position

    p2 =
      tile2.position
  in
  if p1.row < p2.row then
    LT
  else if p1.row > p2.row then
    GT
  else
    compare p2.col p1.col


moveRightHelper : MovementState -> List Tile.State -> MovementState
moveRightHelper state tiles =
  case tiles of
    [] ->
      case state.tileInCell of
        Nothing ->
          state

        Just { id, value, position } ->
          { state
          | lastPosition = position
          , tileInCell = Nothing
          , newTiles =
              state.newTiles ++ [ Tile.old id value position ]
          }

    tile :: restTiles ->
      let
        state1 =
          if tile.position.row > state.lastPosition.row then
            { state
            | lastPosition = Position tile.position.row 4
            , tileInCell = Nothing
            , newTiles =
                (++) state.newTiles <|
                  case state.tileInCell of
                    Nothing ->
                      []

                    Just prevTile ->
                      [ Tile.old prevTile.id prevTile.value prevTile.position
                      ]
            }
          else
            state

        state2 =
          case state1.tileInCell of
            Nothing ->
              { state1
              | tileInCell = Just <| Tile.State tile.id tile.value state1.lastPosition
              }

            Just prevTile ->
              let
                lastPosition =
                  Position
                    state1.lastPosition.row
                    (state1.lastPosition.col - 1)
              in
              if Value.isEqual tile.value prevTile.value then
                { state1
                | newTiles =
                    (++) state1.newTiles <|
                      [ Tile.merged tile.id tile.value prevTile.position
                      , Tile.merged prevTile.id prevTile.value prevTile.position
                      , Tile.composite
                          state1.currentId
                          (Value.double prevTile.value)
                          prevTile.position
                      ]
                , currentId = state1.currentId + 1
                , tileInCell = Nothing
                , lastPosition = lastPosition
                }
              else
                { state1
                | newTiles =
                    (++) state1.newTiles <|
                      [ Tile.old prevTile.id prevTile.value prevTile.position
                      ]
                , tileInCell = Just <| Tile.State tile.id tile.value lastPosition
                , lastPosition = lastPosition
                }
      in
      moveRightHelper state2 restTiles


age : List Tile -> List Tile.State
age =
  List.filterMap Tile.age


unsafeFromTiles : List { value : Value, position : Position } -> Grid
unsafeFromTiles input =
  unsafeFromTilesHelper 0 input []


unsafeFromTilesHelper
  : Int
  -> List { value : Value, position : Position }
  -> List Tile
  -> Grid
unsafeFromTilesHelper currentId input tiles =
  case input of
    [] ->
      Grid { currentId = currentId, tiles = tiles }

    { value, position } :: restInput ->
      unsafeFromTilesHelper
        (currentId + 1)
        restInput
        (tiles ++ [ Tile.new currentId value position ])
