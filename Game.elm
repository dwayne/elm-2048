module Game exposing (main, groupByRowLR, moveUp, moveDown, moveLeft, moveRight)

import Array exposing (Array)
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

model : Model
model =
  { score = 0
  , tiles =
      [ { row = 0, col = 0, value = 2 }
      , { row = 0, col = 1, value = 4 }
      , { row = 0, col = 2, value = 8 }
      , { row = 0, col = 3, value = 16 }
      , { row = 1, col = 0, value = 32 }
      , { row = 1, col = 1, value = 64 }
      , { row = 1, col = 2, value = 128 }
      , { row = 1, col = 3, value = 256 }
      , { row = 2, col = 0, value = 512 }
      , { row = 2, col = 1, value = 1024 }
      , { row = 2, col = 2, value = 2048 }
      ]
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

-- TILE

-- Takes a list of tiles, in any order, that make up the grid and groups them
-- by row such that each row is ordered from left to right. The rows are
-- returned in order from top to bottom and if any row didn't have a tile then
-- that row is simply returned as an empty list.
--
-- Examples:
--
-- groupByRowLR []
-- => [[], [], [], []]
--
-- groupByRowLR
--   [ { row = 3, col = 3, value = 32 }, { row = 0, col = 1, value = 2 }
--   , { row = 2, col = 1, value = 16 }, { row = 2, col = 0, value = 2 }
--   , { row = 0, col = 3, value = 4 }, { row = 3, col = 2, value = 4 }
--   ]
-- =>
-- [ [ { row = 0, col = 1, value = 2 }, { row = 0, col = 3, value = 4 } ]
-- , []
-- , [ { row = 2, col = 0, value = 2 }, { row = 2, col = 1, value = 16 } ]
-- , [ { row = 3, col = 2, value = 4 }, { row = 3, col = 3, value = 32 } ]
-- ]
groupByRowLR : List Tile -> List (List Tile)
groupByRowLR tiles =
  let
    combine : Tile -> Array (List Tile) -> Array (List Tile)
    combine tile groups =
      updateGroup tile.row (insertIntoGroup (lessThan .col) tile) groups
  in
    tiles
      |> List.foldl combine initialGroups
      |> Array.toList

lessThan : (a -> comparable) -> a -> a -> Bool
lessThan f a b =
  f a < f b

initialGroups : Array (List a)
initialGroups =
  Array.repeat cellCount []

insertIntoGroup : (a -> a -> Bool) -> a -> List a -> List a
insertIntoGroup lessThan x list =
  case list of
    [] ->
      [ x ]

    (first :: rest) ->
      if lessThan x first then
        x :: list
      else
        first :: insertIntoGroup lessThan x rest

updateGroup : Int -> (List a -> List a) -> Array (List a) -> Array (List a)
updateGroup index update groups =
  let
    group =
      groups
        |> Array.get index
        |> Maybe.map update
        |> Maybe.withDefault []
  in
    Array.set index group groups

-- Takes a list of tiles, that are in the same column, ordered from top to
-- bottom and moves them to the topmost position they can move. If adjacent
-- tiles have the same value then they are merged (at most once).
--
-- Examples:
--
-- moveUp [ { row = 1, col = 0, value = 2 }, { row = 3, col = 0, value = 4 } ]
-- => [ { row = 1, col = 0, value = 4 }, { row = 0, col = 0, value = 2 } ]
--
-- moveUp [ { row = 1, col = 0, value = 2 }, { row = 3, col = 0, value = 2 } ]
-- => [ { row = 0, col = 0, value = 4 } ]
moveUp : List Tile -> List Tile
moveUp tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> List Tile -> List Tile
    move farthest prev accum tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              accum

            Just prev ->
              prev :: accum

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest + 1) (Just { tile | row = farthest }) accum rest

            Just prev ->
              if prev.value == tile.value then
                move farthest Nothing ({ prev | value = 2 * prev.value } :: accum) rest
              else
                move (farthest + 1) (Just { tile | row = farthest }) (prev :: accum) rest
  in
    move 0 Nothing [] tiles

-- Takes a list of tiles, that are in the same column, ordered from bottom to
-- top and moves them to the bottommost position they can move. If adjacent
-- tiles have the same value then they are merged (at most once).
--
-- Examples:
--
-- moveDown [ { row = 2, col = 0, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- => [ { row = 2, col = 0, value = 2 }, { row = 3, col = 0, value = 4 } ]
--
-- moveDown [ { row = 2, col = 0, value = 2 }, { row = 0, col = 0, value = 2 } ]
-- => [ { row = 3, col = 0, value = 4 } ]
moveDown : List Tile -> List Tile
moveDown tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> List Tile -> List Tile
    move farthest prev accum tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              accum

            Just prev ->
              prev :: accum

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest - 1) (Just { tile | row = farthest }) accum rest

            Just prev ->
              if prev.value == tile.value then
                move farthest Nothing ({ prev | value = 2 * prev.value } :: accum) rest
              else
                move (farthest - 1) (Just { tile | row = farthest }) (prev :: accum) rest
  in
    move (cellCount - 1) Nothing [] tiles

-- Takes a list of tiles, that are in the same row, ordered from left to right
-- and moves them to the farthest left position they can move. If adjacent tiles
-- have the same value then they are merged (at most once).
--
-- Examples:
--
-- moveLeft [ { row = 0, col = 1, value = 2 }, { row = 0, col = 3, value = 4 } ]
-- => [ { row = 0, col = 1, value = 4 }, { row = 0, col = 0, value = 2 } ]
--
-- moveLeft [ { row = 0, col = 1, value = 2 }, { row = 0, col = 3, value = 2 } ]
-- => [ { row = 0, col = 0, value = 4 } ]
moveLeft : List Tile -> List Tile
moveLeft tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> List Tile -> List Tile
    move farthest prev accum tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              accum

            Just prev ->
              prev :: accum

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest + 1) (Just { tile | col = farthest }) accum rest

            Just prev ->
              if prev.value == tile.value then
                move farthest Nothing ({ prev | value = 2 * prev.value } :: accum) rest
              else
                move (farthest + 1) (Just { tile | col = farthest }) (prev :: accum) rest
  in
    move 0 Nothing [] tiles

-- Takes a list of tiles, that are in the same row, ordered from right to left
-- and moves them to the farthest right position they can move. If adjacent tiles
-- have the same value then they are merged (at most once).
--
-- Examples:
--
-- moveRight [ { row = 0, col = 2, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- => [ { row = 0, col = 2, value = 2 }, { row = 0, col = 3, value = 4 } ]
--
-- moveRight [ { row = 0, col = 2, value = 2 }, { row = 0, col = 0, value = 2 } ]
-- => [ { row = 0, col = 3, value = 4 } ]
moveRight : List Tile -> List Tile
moveRight tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> List Tile -> List Tile
    move farthest prev accum tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              accum

            Just prev ->
              prev :: accum

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest - 1) (Just { tile | col = farthest }) accum rest

            Just prev ->
              if prev.value == tile.value then
                move farthest Nothing ({ prev | value = 2 * prev.value } :: accum) rest
              else
                move (farthest - 1) (Just { tile | col = farthest }) (prev :: accum) rest
  in
    move (cellCount - 1) Nothing [] tiles

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
