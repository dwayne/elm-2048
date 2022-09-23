module App.Data.Grid exposing
  ( Grid, empty
  , reset
  , generator
  , toTiles
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
  tiles
