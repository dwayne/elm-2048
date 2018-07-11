module Game.Config exposing
  ( cellCount, cellSize, cellDistance, cellColor
  , minValue, minValueProb, TileInfo, tileInfo
  , gridSize, gridColor
  )

import Dict


-- CELL


-- The number of cells per row (or equivalently, per column).
cellCount : Int
cellCount = 4

-- The length (in px) of one side of a cell.
cellSize : Int
cellSize = 100

-- The length (in px) of the spacing between adjacent cells.
cellSpacing : Int
cellSpacing = 15

-- Calculate the number of pixels away the cell in
-- row/column n (0 <= n < cellCount) is from the
-- top/left of the grid.
cellDistance : Int -> Int
cellDistance n =
  cellSpacing + n * (cellSize + cellSpacing)

cellColor : String
cellColor = "rgba(238, 228, 218, 0.35)"


-- TILE


minValue : Int
minValue = 2

-- The probability for randomly choosing the minimum value.
minValueProb : Float
minValueProb = 0.9

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


-- GRID


-- The length (in px) of one side of the grid.
gridSize : Int
gridSize =
  cellCount * (cellSize + cellSpacing) + cellSpacing

gridColor : String
gridColor = "#bbada0"
