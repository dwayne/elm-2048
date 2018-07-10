module Game.Grid exposing
  ( Grid, Tile
  , empty, fromList
  , toList
  , start, next
  , Direction(..), Movement, move
  , hasMoves, hasWinningTile
  )

import Bitwise
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

fromList : List Tile -> Grid
fromList tiles =
  let
    iter : List Tile -> List Tile -> List Tile
    iter accum tiles =
      case tiles of
        [] ->
          accum

        (tile :: rest) ->
          if valid tile accum then
            iter (tile :: accum) rest
          else
            iter accum rest

    valid : Tile -> List Tile -> Bool
    valid tile tiles =
      inBounds tile.row tile.col && properValue tile.value && notExists tile tiles

    inBounds : Int -> Int -> Bool
    inBounds row col =
      row >= 0 && row < C.cellCount && col >= 0 && col < C.cellCount

    properValue : Int -> Bool
    properValue n =
      n >= C.minValue && n <= C.maxValue && isPower2 n

    isPower2 : Int -> Bool
    isPower2 n =
      Bitwise.and n (n - 1) == 0

    notExists : Tile -> List Tile -> Bool
    notExists tile tiles =
      List.all ((/=) tile) tiles
  in
    Grid (iter [] tiles)


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

-- Generate a grid with one extra tile in an available position.
--
-- N.B. If no positions are available then it returns the original grid.
next : Grid -> Generator Grid
next (Grid tiles) =
  oneTile tiles
    |> Random.map
      (\tile ->
        if List.length tiles == C.cellTotal then
          Grid tiles
        else
          Grid (tile :: tiles)
      )

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


-- MOVE


type Direction
  = Up
  | Down
  | Left
  | Right

type alias Movement =
  { grid : Grid
  , score : Int
  , moved : Bool
  }

-- Takes a grid and moves its tiles in the given direction.
--
-- Examples:
--
-- grid =
--   fromList
--     [ { row = 3, col = 3, value = 32 }, { row = 0, col = 1, value = 2 }
--     , { row = 2, col = 1, value = 16 }, { row = 2, col = 0, value = 2 }
--     , { row = 0, col = 3, value = 4 }, { row = 3, col = 2, value = 4 }
--     ]
--
-- move Up grid
-- =>
-- { grid = fromList
--            [ { row = 0, col = 0, value = 2 }, { row = 0, col = 1, value = 2 }
--            , { row = 0, col = 2, value = 4 }, { row = 0, col = 3, value = 4 }
--            , { row = 1, col = 1, value = 16 }, { row = 1, col = 3, value = 32 }
--            ]
-- , score = 0
-- , moved = True
-- }
-- gridU = .grid (move Up grid)
--
-- move Right gridU
-- =>
-- { grid = fromList
--            [ { row = 0, col = 2, value = 4 }, { row = 0, col = 3, value = 8 }
--            , { row = 1, col = 2, value = 16 }, { row = 1, col = 3, value = 32 }
--            ]
-- , score = 12
-- , moved = True
-- }
-- gridUR = .grid (move Right gridU)
--
-- move Left grid
-- =>
-- { grid = fromList
--            [ { row = 0, col = 0, value = 2 }, { row = 0, col = 1, value = 4 }
--            , { row = 2, col = 0, value = 2 }, { row = 2, col = 1, value = 16 }
--            , { row = 3, col = 0, value = 4 }, { row = 3, col = 1, value = 32 }
--            ]
-- , score = 0
-- , moved = True
-- }
-- gridL = .grid (move Left grid)
--
-- move Down gridL
-- =>
-- { grid = fromList
--            [ { row = 1, col = 1, value = 4 },
--            , { row = 2, col = 0, value = 4 }, { row = 2, col = 1, value = 16 }
--            , { row = 3, col = 0, value = 4 }, { row = 3, col = 1, value = 32 }
--            ]
-- , score = 4
-- , moved = True
-- }
-- gridLD = .grid (move Down gridL)
move : Direction -> Grid -> Movement
move dir (Grid tiles) =
  let
    (newTiles, score, moved) =
      moveTiles dir tiles
  in
    { grid = Grid newTiles, score = score, moved = moved }

moveTiles : Direction -> List Tile -> (List Tile, Int, Bool)
moveTiles dir tiles =
  let
    combine : List (List Tile, Int, Bool) -> (List Tile, Int, Bool)
    combine =
      List.foldl
        (\(nextTiles, nextScore, nextMoved) (allTiles, allScore, allMoved) ->
          ( nextTiles ++ allTiles
          , nextScore + allScore
          , nextMoved || allMoved
          )
        )
        ([], 0, False)
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
-- tiles have the same value then they are merged (at most once).
--
-- Examples:
--
-- moveUp [ { row = 1, col = 0, value = 2 }, { row = 3, col = 0, value = 4 } ]
-- =>
-- ( [ { row = 1, col = 0, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- , 0
-- , True
-- )
--
-- moveUp [ { row = 1, col = 0, value = 2 }, { row = 3, col = 0, value = 2 } ]
-- =>
-- ( [ { row = 0, col = 0, value = 4 } ]
-- , 4
-- , True
-- )
--
-- moveUp [ { row = 0, col = 0, value = 4 } ]
-- =>
-- ( [ { row = 0, col = 0, value = 4 } ]
-- , 0
-- , False
-- )
moveUp : List Tile -> (List Tile, Int, Bool)
moveUp tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> Bool -> List Tile -> (List Tile, Int, Bool)
    move farthest prev accum score moved tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              (accum, score, moved)

            Just prev ->
              (prev :: accum, score, moved)

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest + 1) (Just { tile | row = farthest }) accum score (moved || tile.row /= farthest) rest

            Just prev ->
              if prev.value == tile.value then
                let
                  newValue =
                    2 * prev.value
                in
                  move farthest Nothing ({ prev | value = newValue } :: accum) (score + newValue) True rest
              else
                move (farthest + 1) (Just { tile | row = farthest }) (prev :: accum) score (moved || tile.row /= farthest) rest
  in
    move 0 Nothing [] 0 False tiles

-- Takes a list of tiles, that are in the same column, ordered from bottom to
-- top and moves them to the bottommost position they can move. If adjacent
-- tiles have the same value then they are merged (at most once).
--
-- Examples:
--
-- moveDown [ { row = 2, col = 0, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- =>
-- ( [ { row = 2, col = 0, value = 2 }, { row = 3, col = 0, value = 4 } ]
-- , 0
-- , True
-- )
--
-- moveDown [ { row = 2, col = 0, value = 2 }, { row = 0, col = 0, value = 2 } ]
-- =>
-- ( [ { row = 3, col = 0, value = 4 } ]
-- , 4
-- , True
-- )
--
-- moveDown [ { row = 3, col = 0, value = 4 } ]
-- =>
-- ( [ { row = 3, col = 0, value = 4 } ]
-- , 0
-- , False
-- )
moveDown : List Tile -> (List Tile, Int, Bool)
moveDown tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> Bool -> List Tile -> (List Tile, Int, Bool)
    move farthest prev accum score moved tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              (accum, score, moved)

            Just prev ->
              (prev :: accum, score, moved)

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest - 1) (Just { tile | row = farthest }) accum score (moved || tile.row /= farthest) rest

            Just prev ->
              if prev.value == tile.value then
                let
                  newValue =
                    2 * prev.value
                in
                  move farthest Nothing ({ prev | value = newValue } :: accum) (score + newValue) True rest
              else
                move (farthest - 1) (Just { tile | row = farthest }) (prev :: accum) score (moved || tile.row /= farthest) rest
  in
    move (C.cellCount - 1) Nothing [] 0 False tiles

-- Takes a list of tiles, that are in the same row, ordered from left to right
-- and moves them to the farthest left position they can move. If adjacent tiles
-- have the same value then they are merged (at most once).
--
-- Examples:
--
-- moveLeft [ { row = 0, col = 1, value = 2 }, { row = 0, col = 3, value = 4 } ]
-- =>
-- ( [ { row = 0, col = 1, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- , 0
-- , True
-- )
--
-- moveLeft [ { row = 0, col = 1, value = 2 }, { row = 0, col = 3, value = 2 } ]
-- =>
-- ( [ { row = 0, col = 0, value = 4 } ]
-- , 4
-- , True
-- )
--
-- moveLeft [ { row = 0, col = 0, value = 4 } ]
-- =>
-- ( [ { row = 0, col = 0, value = 4 } ]
-- , 0
-- , False
-- )
moveLeft : List Tile -> (List Tile, Int, Bool)
moveLeft tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> Bool -> List Tile -> (List Tile, Int, Bool)
    move farthest prev accum score moved tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              (accum, score, moved)

            Just prev ->
              (prev :: accum, score, moved)

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest + 1) (Just { tile | col = farthest }) accum score (moved || tile.col /= farthest) rest

            Just prev ->
              if prev.value == tile.value then
                let
                  newValue =
                    2 * prev.value
                in
                  move farthest Nothing ({ prev | value = newValue } :: accum) (score + newValue) True rest
              else
                move (farthest + 1) (Just { tile | col = farthest }) (prev :: accum) score (moved || tile.col /= farthest) rest
  in
    move 0 Nothing [] 0 False tiles

-- Takes a list of tiles, that are in the same row, ordered from right to left
-- and moves them to the farthest right position they can move. If adjacent tiles
-- have the same value then they are merged (at most once).
--
-- Examples:
--
-- moveRight [ { row = 0, col = 2, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- =>
-- ( [ { row = 0, col = 2, value = 2 }, { row = 0, col = 3, value = 4 } ]
-- , 0
-- , True
-- )
--
-- moveRight [ { row = 0, col = 2, value = 2 }, { row = 0, col = 0, value = 2 } ]
-- =>
-- ( [ { row = 0, col = 3, value = 4 } ]
-- , 4
-- , True
-- )
--
-- moveRight [ { row = 0, col = 3, value = 4 } ]
-- =>
-- ( [ { row = 0, col = 3, value = 4 } ]
-- , 0
-- , False
-- )
moveRight : List Tile -> (List Tile, Int, Bool)
moveRight tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> Bool -> List Tile -> (List Tile, Int, Bool)
    move farthest prev accum score moved tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              (accum, score, moved)

            Just prev ->
              (prev :: accum, score, moved)

        (tile :: rest) ->
          case prev of
            Nothing ->
              move (farthest - 1) (Just { tile | col = farthest }) accum score (moved || tile.col /= farthest) rest

            Just prev ->
              if prev.value == tile.value then
                let
                  newValue =
                    2 * prev.value
                in
                  move farthest Nothing ({ prev | value = newValue } :: accum) (score + newValue) True rest
              else
                move (farthest - 1) (Just { tile | col = farthest }) (prev :: accum) score (moved || tile.col /= farthest) rest
  in
    move (C.cellCount - 1) Nothing [] 0 False tiles


-- QUERY


-- Determines whether a move can be made.
--
-- A move can be made if there is at least one cell not occupied by a tile or
-- if we can merge one tile onto another.
hasMoves : Grid -> Bool
hasMoves (Grid tiles) =
  hasAvailableCells tiles || canMergeTiles tiles

hasAvailableCells : List Tile -> Bool
hasAvailableCells =
  not << List.isEmpty << availableCells

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

canMergeTiles : List Tile -> Bool
canMergeTiles tiles =
  let
    possibleMoves : Tile -> List Tile
    possibleMoves tile =
      [ { tile | row = tile.row - 1 }
      , { tile | col = tile.col - 1 }
      , { tile | col = tile.col + 1 }
      , { tile | row = tile.row + 1 }
      ]

    hasMergeableTiles : List Tile -> List Tile -> Bool
    hasMergeableTiles a b =
      List.any (\tile -> List.any ((==) tile) b) a
  in
    case tiles of
      [] ->
        False

      (first :: rest) ->
        hasMergeableTiles (possibleMoves first) rest || canMergeTiles rest

hasWinningTile : Grid -> Bool
hasWinningTile (Grid tiles) =
  List.any (\tile -> tile.value == C.maxValue) tiles
