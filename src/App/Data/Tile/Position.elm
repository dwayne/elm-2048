module App.Data.Tile.Position exposing (Position, comparator, generator)


import App.Lib.Random as Random
import Random
import Set exposing (Set)


type alias Position =
  { row : Int
  , col : Int
  }


type alias PositionTuple =
  (Int, Int)


comparator : Position -> Position -> Order
comparator pos1 pos2 =
  if pos1.row < pos2.row then
    LT
  else if pos1.row > pos2.row then
    GT
  else
    compare pos1.col pos2.col


-- It generates at most 2 random positions.
generator : List Position -> Random.Generator (List Position)
generator unavailablePositions =
  let
    availablePositions =
      unavailablePositions
        |> List.map toPositionTuple
        |> Set.fromList
        |> Set.diff allPositionTuples
        |> Set.toList
        |> List.map toPosition
  in
  Random.selectAtMostN 2 availablePositions
    |> Random.map Tuple.first


toPositionTuple : Position -> PositionTuple
toPositionTuple { row, col } =
  (row, col)


toPosition : PositionTuple -> Position
toPosition (row, col) =
  { row = row, col = col }


allPositionTuples : Set PositionTuple
allPositionTuples =
  [ (1, 1), (1, 2), (1, 3), (1, 4)
  , (2, 1), (2, 2), (2, 3), (2, 4)
  , (3, 1), (3, 2), (3, 3), (3, 4)
  , (4, 1), (4, 2), (4, 3), (4, 4)
  ]
  |> Set.fromList
