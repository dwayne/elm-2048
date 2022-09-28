module App.Data.Tile.Position exposing
  ( Position
  , availablePositions
  , selectAtMost2
  )


import App.Lib.Random exposing (selectAtMostN)
import Random
import Set exposing (Set)


type alias Position =
  { row : Int
  , col : Int
  }


availablePositions : List Position -> List Position
availablePositions unavailablePositions =
  unavailablePositions
    |> List.map (\{ row, col } -> (row, col))
    |> Set.fromList
    |> Set.diff allPositions
    |> Set.toList
    |> List.map (\(row, col) -> Position row col)


selectAtMost2 : List Position -> Random.Generator (List Position)
selectAtMost2 =
  availablePositions >> selectAtMostN 2 >> Random.map Tuple.first


allPositions : Set (Int, Int)
allPositions =
  Set.fromList
    [ (1, 1), (1, 2), (1, 3), (1, 4)
    , (2, 1), (2, 2), (2, 3), (2, 4)
    , (3, 1), (3, 2), (3, 3), (3, 4)
    , (4, 1), (4, 2), (4, 3), (4, 4)
    ]
