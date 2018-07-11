module Game.Grid exposing
  ( Grid, Tile
  , empty
  , toList
  , start
  )

import Random exposing (Generator)

import Game.Config as C
import Game.List exposing (cartesianMap)


-- GRID


type Grid
  = Grid (List Tile)

type alias Tile =
  { row : Int
  , col : Int
  , value : Int
  }


-- CREATING GRIDS


empty : Grid
empty =
  Grid []


-- TAKING GRIDS APART


toList : Grid -> List Tile
toList (Grid tiles) =
  tiles


-- GENERATORS


-- Generate a grid with two tiles in any of the available positions.
start : Generator Grid
start =
  twoTiles []
    |> Random.map (\(tile1, tile2) -> Grid [tile1, tile2])

-- Generate one tile in an available position.
--
-- N.B. It assumes that at least one position is available.
oneTile : List Tile -> Generator Tile
oneTile tiles =
  let
    get : Int -> List (Int, Int) -> (Int, Int)
    get index list =
      list
        |> List.drop index
        |> List.head
        |> Maybe.withDefault (0, 0)

    possibilities =
      availableCells tiles

    maxIndex =
      List.length possibilities - 1
  in
    Random.pair (Random.int 0 maxIndex) valueGen
      |> Random.map
        (\(index, value) ->
          let
            (row, col) =
              get index possibilities
          in
            { row = row, col = col, value = value }
        )

-- Generate two tiles in available positions.
--
-- N.B. It assumes that at least two positions are available.
twoTiles : List Tile -> Generator (Tile, Tile)
twoTiles tiles =
  oneTile tiles
    |> Random.andThen
      (\tile ->
        oneTile (tile :: tiles)
          |> Random.map ((,) tile)
      )

valueGen : Generator Int
valueGen =
  Random.float 0 1
    |> Random.map (\x -> if x < C.minValueProb then C.minValue else 2 * C.minValue)


-- QUERY


-- Determines all the available cell positions.
--
-- For e.g.
--
-- availableCells
--   [ { row = 0, col = 1, value = 4 }, { row = 2, col = 3, value = 16 } ]
-- =>
-- [ (0, 0), (0, 2), (0, 3)
-- , (1, 0), (1, 1), (1, 2), (1, 3)
-- , (2, 0), (2, 1), (2, 2)
-- , (3, 0), (3, 1), (3, 2), (3, 3)
-- ]
availableCells : List Tile -> List (Int, Int)
availableCells tiles =
  let
    allPositions =
      cartesianMap C.cellCount C.cellCount (,)
  in
    List.filter
      (\(row, col) ->
        List.all
          (\tile -> tile.row /= row || tile.col /= col)
          tiles
      )
      allPositions
