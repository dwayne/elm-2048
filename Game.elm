module Game exposing (main, groupByRowLR, groupByRowRL, groupByColTB, groupByColBT, moveUp, moveDown, moveLeft, moveRight)

import Dict
import Html exposing (..)
import Svg exposing (Svg)
import Svg.Attributes

main : Html msg
main =
  view model

-- MODEL

type alias Model =
  { score : Int
  , tiles : List Tile
  }

type alias Tile =
  { row : Int
  , col : Int
  , value : Int
  }

tiles : List Tile
tiles =
  [ { row = 3, col = 3, value = 32 }, { row = 0, col = 1, value = 2 }
  , { row = 2, col = 1, value = 16 }, { row = 2, col = 0, value = 2 }
  , { row = 0, col = 3, value = 4 }, { row = 3, col = 2, value = 4 }
  ]

tilesU : List Tile
tilesU =
  Tuple.first (move Up tiles)

tilesUR : List Tile
tilesUR =
  Tuple.first (move Right tilesU)

tilesL : List Tile
tilesL =
  Tuple.first (move Left tiles)

tilesLD : List Tile
tilesLD =
  Tuple.first (move Down tilesL)

model : Model
model =
  { score = Tuple.second (move Right tilesU)
    -- Put one of tiles, tilesU, tilesUR, tilesL, or tilesLD here to see how
    -- move works.
  , tiles = tilesUR
  }

-- VIEW

view : Model -> Html msg
view { score, tiles } =
  div []
    [ h1 [] [ text "Elm 2048" ]
    , p [] [ text <| "Score: " ++ (toString score) ]
    , p [] [ button [] [ text "New Game" ] ]
    , viewGrid tiles
    ]

-- GRID

cellCount : Int
cellCount = 4

cellSize : Int
cellSize = 100

cellSpacing : Int
cellSpacing = 15

-- Calculate the number of pixels away the cell in row/column
-- n (0 <= n < cellCount) is from the top/left of the grid.
cellDistance : Int -> Int
cellDistance n =
  cellSpacing + n * (cellSize + cellSpacing)

cellColor : String
cellColor = "rgba(238, 228, 218, 0.35)"

gridSize : Int
gridSize =
  cellCount * (cellSize + cellSpacing) + cellSpacing

gridColor : String
gridColor = "#bbada0"

viewGrid : List Tile -> Html msg
viewGrid tiles =
  let
    size =
      toString gridSize
  in
    Svg.svg
      [ Svg.Attributes.width size
      , Svg.Attributes.height size
      , Svg.Attributes.viewBox ("0 0 " ++ size ++ " " ++ size)
      , Svg.Attributes.style ("background: " ++ gridColor)
      ]
      [ viewCells
      , viewTiles tiles
      ]

viewCells : Svg msg
viewCells =
  Svg.g [ Svg.Attributes.fill cellColor ]
    <| cartesianMap cellCount cellCount viewCell

viewCell : Int -> Int -> Svg msg
viewCell row col =
  let
    x =
      cellDistance col

    y =
      cellDistance row

    size =
      toString cellSize
  in
    Svg.rect
      [ Svg.Attributes.x (toString x)
      , Svg.Attributes.y (toString y)
      , Svg.Attributes.width size
      , Svg.Attributes.height size
      ]
      []

type Direction
  = Up
  | Down
  | Left
  | Right

-- Takes a list of tiles, in any order, and moves them in the given direction.
-- The resulting tiles and the score earned is returned.
--
-- Examples:
--
-- tiles =
--   [ { row = 3, col = 3, value = 32 }, { row = 0, col = 1, value = 2 }
--   , { row = 2, col = 1, value = 16 }, { row = 2, col = 0, value = 2 }
--   , { row = 0, col = 3, value = 4 }, { row = 3, col = 2, value = 4 }
--   ]
--
-- move Up tiles
-- =>
-- ( [ { row = 0, col = 0, value = 2 }, { row = 0, col = 1, value = 2 }
--   , { row = 0, col = 2, value = 4 }, { row = 0, col = 3, value = 4 }
--   , { row = 1, col = 1, value = 16 }, { row = 1, col = 3, value = 32 }
--   ]
-- , 0
-- )
-- tilesU = Tuple.first (move Up tiles)
--
-- move Right tilesU
-- =>
-- ( [ { row = 0, col = 2, value = 4 }, { row = 0, col = 3, value = 8 }
--   , { row = 1, col = 2, value = 16 }, { row = 1, col = 3, value = 32 }
--   ]
-- , 12
-- )
-- tilesUR = Tuple.first (move Right tilesU)
--
-- move Left tiles
-- =>
-- ( [ { row = 0, col = 0, value = 2 }, { row = 0, col = 1, value = 4 }
--   , { row = 2, col = 0, value = 2 }, { row = 2, col = 1, value = 16 }
--   , { row = 3, col = 0, value = 4 }, { row = 3, col = 1, value = 32 }
--   ]
-- , 0
-- )
-- tilesL = Tuple.first (move Left tiles)
--
-- move Down tilesL
-- =>
-- ( [ { row = 1, col = 1, value = 4 },
--   , { row = 2, col = 0, value = 4 }, { row = 2, col = 1, value = 16 }
--   , { row = 3, col = 0, value = 4 }, { row = 3, col = 1, value = 32 }
--   ]
-- , 4
-- )
-- tilesLD = Tuple.first (move Down tilesL)
--
-- N.B. The order of tiles, tilesU, tilesUR, tilesL and tilesLD doesn't matter.
move : Direction -> List Tile -> (List Tile, Int)
move dir tiles =
  let
    combine : List (List Tile, Int) -> (List Tile, Int)
    combine =
      List.foldl
        (\(tiles, score) (allTiles, totalScore) ->
          (tiles ++ allTiles, score + totalScore)
        )
        ([], 0)
  in
    case dir of
      Up ->
        tiles
          |> groupByColTB
          |> List.map moveUp
          |> combine

      Down ->
        tiles
          |> groupByColBT
          |> List.map moveDown
          |> combine

      Left ->
        tiles
          |> groupByRowLR
          |> List.map moveLeft
          |> combine

      Right ->
        tiles
          |> groupByRowRL
          |> List.map moveRight
          |> combine

-- TILE

-- Takes a list of tiles, in any order, that make up the grid and groups them
-- by row from top to bottom such that each row is ordered from left to right.
-- If a row doesn't have any tiles then nothing is returned for that row.
--
-- Examples:
--
-- groupByRowLR []
-- => []
--
-- groupByRowLR
--   [ { row = 3, col = 3, value = 32 }, { row = 0, col = 1, value = 2 }
--   , { row = 2, col = 1, value = 16 }, { row = 2, col = 0, value = 2 }
--   , { row = 0, col = 3, value = 4 }, { row = 3, col = 2, value = 4 }
--   ]
-- =>
-- [ [ { row = 0, col = 1, value = 2 }, { row = 0, col = 3, value = 4 } ]
-- , [ { row = 2, col = 0, value = 2 }, { row = 2, col = 1, value = 16 } ]
-- , [ { row = 3, col = 2, value = 4 }, { row = 3, col = 3, value = 32 } ]
-- ]
--
-- Notice how row = 1 is missing since it had no tiles in it.
groupByRowLR : List Tile -> List (List Tile)
groupByRowLR tiles =
  let
    -- Bottom to top, right to left
    btrl : Tile -> Tile -> Order
    btrl tile1 tile2 =
      case compare tile1.row tile2.row of
        EQ ->
          compare tile2.col tile1.col

        LT ->
          GT

        GT ->
          LT
  in
    tiles
      |> List.sortWith btrl
      |> groupByRow

-- Takes a list of tiles, in any order, that make up the grid and groups them
-- by row from bottom to top such that each row is ordered from right to left.
-- If a row doesn't have any tiles then nothing is returned for that row.
--
-- Examples:
--
-- groupByRowRL []
-- => []
--
-- groupByRowRL
--   [ { row = 3, col = 3, value = 32 }, { row = 0, col = 1, value = 2 }
--   , { row = 2, col = 1, value = 16 }, { row = 2, col = 0, value = 2 }
--   , { row = 0, col = 3, value = 4 }, { row = 3, col = 2, value = 4 }
--   ]
-- =>
-- [ [ { row = 3, col = 3, value = 32 }, { row = 3, col = 2, value = 4 } ]
-- , [ { row = 2, col = 1, value = 16 }, { row = 2, col = 0, value = 2 } ]
-- , [ { row = 0, col = 3, value = 4 }, { row = 0, col = 1, value = 2 } ]
-- ]
--
-- Notice how row = 1 is missing since it had no tiles in it. Also, this one
-- returns the rows bottom to top but it doesn't matter in the end. We can do
-- a List.reverse to get it top to bottom if it does become necessary. All that
-- matters is that we have the rows grouped and within a group they are in the
-- correct order.
groupByRowRL : List Tile -> List (List Tile)
groupByRowRL tiles =
  let
    -- Top to bottom, left to right
    tblr : Tile -> Tile -> Order
    tblr tile1 tile2 =
      case compare tile1.row tile2.row of
        EQ ->
          compare tile1.col tile2.col

        LT ->
          LT

        GT ->
          GT
  in
    tiles
      |> List.sortWith tblr
      |> groupByRow

groupByRow : List Tile -> List (List Tile)
groupByRow tiles =
  let
    iter : List Tile -> List (List Tile) -> List Tile -> List (List Tile)
    iter group groups tiles =
      case tiles of
        [] ->
          if List.isEmpty group then
            groups
          else
            group :: groups

        (first :: rest) ->
          case group of
            [] ->
              iter [ first ] groups rest

            (tile :: _) ->
              if first.row == tile.row then
                iter (first :: group) groups rest
              else
                iter [ first ] (group :: groups) rest
  in
    iter [] [] tiles

-- Takes a list of tiles, in any order, that make up the grid and groups them
-- by column such that each column is ordered from top to bottom.
-- If a column doesn't have any tiles then nothing is returned for that column.
--
-- Examples:
--
-- groupByColTB []
-- => []
--
-- groupByColTB
--   [ { row = 3, col = 3, value = 32 }, { row = 0, col = 1, value = 2 }
--   , { row = 2, col = 1, value = 16 }, { row = 2, col = 0, value = 2 }
--   , { row = 0, col = 3, value = 4 }, { row = 3, col = 2, value = 4 }
--   ]
-- =>
-- [ [ { row = 2, col = 0, value = 2 } ]
-- , [ { row = 0, col = 1, value = 2 }, { row = 2, col = 1, value = 16 } ]
-- , [ { row = 3, col = 2, value = 4 } ]
-- , [ { row = 0, col = 3, value = 4 }, { row = 3, col = 3, value = 32 } ]
-- ]
groupByColTB : List Tile -> List (List Tile)
groupByColTB tiles =
  let
    -- Right to left, bottom to top
    rlbt : Tile -> Tile -> Order
    rlbt tile1 tile2 =
      case compare tile1.col tile2.col of
        EQ ->
          compare tile2.row tile1.row

        LT ->
          GT

        GT ->
          LT
  in
    tiles
      |> List.sortWith rlbt
      |> groupByCol

-- Takes a list of tiles, in any order, that make up the grid and groups them
-- by column such that each column is ordered from bottom to top.
-- If a column doesn't have any tiles then nothing is returned for that column.
--
-- Examples:
--
-- groupByColBT []
-- => []
--
-- groupByColBT
--   [ { row = 3, col = 3, value = 32 }, { row = 0, col = 1, value = 2 }
--   , { row = 2, col = 1, value = 16 }, { row = 2, col = 0, value = 2 }
--   , { row = 0, col = 3, value = 4 }, { row = 3, col = 2, value = 4 }
--   ]
-- =>
-- [ [ { row = 3, col = 3, value = 32 }, { row = 0, col = 3, value = 4 } ]
-- , [ { row = 3, col = 2, value = 4 } ]
-- , [ { row = 2, col = 1, value = 16 }, { row = 0, col = 1, value = 2 } ]
-- , [ { row = 2, col = 0, value = 2 } ]
-- ]
groupByColBT : List Tile -> List (List Tile)
groupByColBT tiles =
  let
    -- Left to right, top to bottom
    lrtb : Tile -> Tile -> Order
    lrtb tile1 tile2 =
      case compare tile1.col tile2.col of
        EQ ->
          compare tile1.row tile2.row

        LT ->
          LT

        GT ->
          GT
  in
    tiles
      |> List.sortWith lrtb
      |> groupByCol

groupByCol : List Tile -> List (List Tile)
groupByCol tiles =
  let
    iter : List Tile -> List (List Tile) -> List Tile -> List (List Tile)
    iter group groups tiles =
      case tiles of
        [] ->
          if List.isEmpty group then
            groups
          else
            group :: groups

        (first :: rest) ->
          case group of
            [] ->
              iter [ first ] groups rest

            (tile :: _) ->
              if first.col == tile.col then
                iter (first :: group) groups rest
              else
                iter [ first ] (group :: groups) rest
  in
    iter [] [] tiles

-- Takes a list of tiles, that are in the same column, ordered from top to
-- bottom and moves them to the topmost position they can move. If adjacent
-- tiles have the same value then they are merged (at most once). It also keeps
-- track of the score earned.
--
-- Examples:
--
-- moveUp [ { row = 1, col = 0, value = 2 }, { row = 3, col = 0, value = 4 } ]
-- =>
-- ( [ { row = 1, col = 0, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- , 0
-- )
--
-- moveUp [ { row = 1, col = 0, value = 2 }, { row = 3, col = 0, value = 2 } ]
-- =>
-- ( [ { row = 0, col = 0, value = 4 } ]
-- , 4
-- )
moveUp : List Tile -> (List Tile, Int)
moveUp tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> List Tile -> (List Tile, Int)
    move farthest prev accum score tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              (accum, score)

            Just prev ->
              (prev :: accum, score)

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest + 1) (Just { tile | row = farthest }) accum score rest

            Just prev ->
              if prev.value == tile.value then
                let
                  newValue =
                    2 * prev.value
                in
                  move farthest Nothing ({ prev | value = newValue } :: accum) (score + newValue) rest
              else
                move (farthest + 1) (Just { tile | row = farthest }) (prev :: accum) score rest
  in
    move 0 Nothing [] 0 tiles

-- Takes a list of tiles, that are in the same column, ordered from bottom to
-- top and moves them to the bottommost position they can move. If adjacent
-- tiles have the same value then they are merged (at most once). It also keeps
-- track of the score earned.
--
-- Examples:
--
-- moveDown [ { row = 2, col = 0, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- =>
-- ( [ { row = 2, col = 0, value = 2 }, { row = 3, col = 0, value = 4 } ]
-- , 0
-- )
--
-- moveDown [ { row = 2, col = 0, value = 2 }, { row = 0, col = 0, value = 2 } ]
-- =>
-- ( [ { row = 3, col = 0, value = 4 } ]
-- , 4
-- )
moveDown : List Tile -> (List Tile, Int)
moveDown tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> List Tile -> (List Tile, Int)
    move farthest prev accum score tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              (accum, score)

            Just prev ->
              (prev :: accum, score)

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest - 1) (Just { tile | row = farthest }) accum score rest

            Just prev ->
              if prev.value == tile.value then
                let
                  newValue =
                    2 * prev.value
                in
                  move farthest Nothing ({ prev | value = newValue } :: accum) (score + newValue) rest
              else
                move (farthest - 1) (Just { tile | row = farthest }) (prev :: accum) score rest
  in
    move (cellCount - 1) Nothing [] 0 tiles

-- Takes a list of tiles, that are in the same row, ordered from left to right
-- and moves them to the farthest left position they can move. If adjacent tiles
-- have the same value then they are merged (at most once). It also keeps track
-- of the score earned.
--
-- Examples:
--
-- moveLeft [ { row = 0, col = 1, value = 2 }, { row = 0, col = 3, value = 4 } ]
-- =>
-- ( [ { row = 0, col = 1, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- , 0
-- )
--
-- moveLeft [ { row = 0, col = 1, value = 2 }, { row = 0, col = 3, value = 2 } ]
-- =>
-- ( [ { row = 0, col = 0, value = 4 } ]
-- , 4
-- )
moveLeft : List Tile -> (List Tile, Int)
moveLeft tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> List Tile -> (List Tile, Int)
    move farthest prev accum score tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              (accum, score)

            Just prev ->
              (prev :: accum, score)

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest + 1) (Just { tile | col = farthest }) accum score rest

            Just prev ->
              if prev.value == tile.value then
                let
                  newValue =
                    2 * prev.value
                in
                  move farthest Nothing ({ prev | value = newValue } :: accum) (score + newValue) rest
              else
                move (farthest + 1) (Just { tile | col = farthest }) (prev :: accum) score rest
  in
    move 0 Nothing [] 0 tiles

-- Takes a list of tiles, that are in the same row, ordered from right to left
-- and moves them to the farthest right position they can move. If adjacent tiles
-- have the same value then they are merged (at most once). It also keeps track
-- of the score earned.
--
-- Examples:
--
-- moveRight [ { row = 0, col = 2, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- =>
-- ( [ { row = 0, col = 2, value = 2 }, { row = 0, col = 3, value = 4 } ]
-- , 0
-- )
--
-- moveRight [ { row = 0, col = 2, value = 2 }, { row = 0, col = 0, value = 2 } ]
-- =>
-- ( [ { row = 0, col = 3, value = 4 } ]
-- , 4
-- )
moveRight : List Tile -> (List Tile, Int)
moveRight tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> List Tile -> (List Tile, Int)
    move farthest prev accum score tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              (accum, score)

            Just prev ->
              (prev :: accum, score)

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest - 1) (Just { tile | col = farthest }) accum score rest

            Just prev ->
              if prev.value == tile.value then
                let
                  newValue =
                    2 * prev.value
                in
                  move farthest Nothing ({ prev | value = newValue } :: accum) (score + newValue) rest
              else
                move (farthest - 1) (Just { tile | col = farthest }) (prev :: accum) score rest
  in
    move (cellCount - 1) Nothing [] 0 tiles

type alias TileInfo =
  { color : String
  , textColor : String
  , fontSize : String
  }

tileInfoDict : Dict.Dict Int TileInfo
tileInfoDict =
  Dict.fromList
    [ (2, { color = "#eee4da", textColor = "#776e65", fontSize = "55px" })
    , (4, { color = "#ede0c8", textColor = "#776e65", fontSize = "55px" })
    , (8, { color = "#f2b179", textColor = "#f9f6f2", fontSize = "55px" })
    , (16, { color = "#f59563", textColor = "#f9f6f2", fontSize = "55px" })
    , (32, { color = "#f67c5f", textColor = "#f9f6f2", fontSize = "55px" })
    , (64, { color = "#f65e3b", textColor = "#f9f6f2", fontSize = "55px" })
    , (128, { color = "#edcf72", textColor = "#f9f6f2", fontSize = "42px" })
    , (256, { color = "#edcc61", textColor = "#f9f6f2", fontSize = "42px" })
    , (512, { color = "#edc850", textColor = "#f9f6f2", fontSize = "42px" })
    , (1024, { color = "#edc53f", textColor = "#f9f6f2", fontSize = "32px" })
    , (2048, { color = "#edc22e", textColor = "#f9f6f2", fontSize = "32px" })
    ]

tileInfo : Int -> TileInfo
tileInfo value =
  let
    default =
      { color = "#eee4da", textColor = "#776e65", fontSize = "55px" }
  in
    tileInfoDict
      |> Dict.get value
      |> Maybe.withDefault default

viewTiles : List Tile -> Svg msg
viewTiles tiles =
  Svg.g [] (List.map viewTile tiles)

viewTile : Tile -> Svg msg
viewTile { row, col, value } =
  let
    x =
      cellDistance col

    y =
      cellDistance row

    size =
      toString cellSize

    halfCellSize =
      cellSize // 2

    textX =
      x + halfCellSize

    textY =
      y + halfCellSize

    info =
      tileInfo value
  in
    Svg.g []
      [ Svg.rect
          [ Svg.Attributes.x (toString x)
          , Svg.Attributes.y (toString y)
          , Svg.Attributes.width size
          , Svg.Attributes.height size
          , Svg.Attributes.fill info.color
          ]
          []
      , Svg.text_
          [ Svg.Attributes.x (toString textX)
          , Svg.Attributes.y (toString textY)
          , Svg.Attributes.fontSize info.fontSize
          , Svg.Attributes.fontWeight "bold"
          , Svg.Attributes.textAnchor "middle"
          , Svg.Attributes.dominantBaseline "central"
          , Svg.Attributes.fill info.textColor
          ]
          [ Svg.text (toString value) ]
      ]

-- HELPERS

-- For e.g.
-- cartesianMap 2 3 (,) = [ (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2) ]
cartesianMap : Int -> Int -> (Int -> Int -> a) -> List a
cartesianMap n m f =
  let
    loop i j =
      if i == n then
        []
      else if j == n then
        loop (i+1) 0
      else
        f i j :: loop i (j+1)
  in
    loop 0 0
