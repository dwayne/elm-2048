module App.Data.Tile.Position exposing (Position, generator)


import App.Lib.Random as Random
import Random
import Set exposing (Set)


type alias Position =
  (Int, Int)


-- It generates at most 2 random positions.
generator : List Position -> Random.Generator (List Position)
generator unavailablePositions =
  let
    availablePositions =
      unavailablePositions
        |> Set.fromList
        |> Set.diff allPositions
        |> Set.toList
  in
  Random.selectAtMostN 2 availablePositions
    |> Random.map Tuple.first


allPositions : Set Position
allPositions =
  Set.fromList
    [ (1, 1), (1, 2), (1, 3), (1, 4)
    , (2, 1), (2, 2), (2, 3), (2, 4)
    , (3, 1), (3, 2), (3, 3), (3, 4)
    , (4, 1), (4, 2), (4, 3), (4, 4)
    ]
