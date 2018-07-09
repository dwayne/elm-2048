module Game exposing (main)

import Dict
import Html exposing (..)
import Html.Events as Events
import Keyboard
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

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

init : (Model, Cmd Msg)
init =
  { score = 0
  , tiles = []
  } ! [ newGrid ]

-- UPDATE

type Msg
  = KeyDown Keyboard.KeyCode
  | NewGame
  | NewGrid (List Tile)
  | NextGrid (List Tile)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown code ->
      let
        direction : Maybe Direction
        direction =
          case code of
            37 ->
              Just Left

            38 ->
              Just Up

            39 ->
              Just Right

            40 ->
              Just Down

            _ ->
              Nothing

        movement : Maybe Movement
        movement =
          Maybe.map (\dir -> move dir model.tiles) direction
      in
        case movement of
          Nothing ->
            model ! []

          Just { score, tiles, moved } ->
            if moved then
              { model | score = model.score + score, tiles = tiles } ! [ nextGrid tiles ]
            else
              model ! []

    NewGame ->
      { model | score = 0 } ! [ newGrid ]

    NewGrid tiles ->
      { model | tiles = tiles } ! []

    NextGrid tiles ->
      let
        nextModel =
          { model | tiles = tiles }
      in
        if hasMoves tiles then
          nextModel ! []
        else
          Debug.log "Game over" nextModel ! []

newGrid : Cmd Msg
newGrid =
  Random.generate NewGrid startGridGen

nextGrid : List Tile -> Cmd Msg
nextGrid tiles =
  Random.generate NextGrid (nextGridGen tiles)

-- VIEW

view : Model -> Html Msg
view { score, tiles } =
  div []
    [ h1 [] [ text "Elm 2048" ]
    , p [] [ text <| "Score: " ++ (toString score) ]
    , p [] [ button [ Events.onClick NewGame ] [ text "New Game" ] ]
    , viewGrid tiles
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Keyboard.downs KeyDown

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
      cartesianMap cellCount cellCount (,)
  in
    List.filter
      (\(row, col) ->
        List.all
          (\tile -> tile.row /= row || tile.col /= col)
          tiles
      )
      allPositions

-- Determines whether a move can be made.
--
-- A move can be made if there is at least one cell not occupied by a tile or
-- if we can merge one tile onto another.
hasMoves : List Tile -> Bool
hasMoves tiles =
  let
    hasCells =
      not (List.isEmpty (availableCells tiles))
  in
    hasCells || hasTileMatches tiles

hasTileMatches : List Tile -> Bool
hasTileMatches tiles =
  let
    possibleMoves : Tile -> List Tile
    possibleMoves tile =
      [ { tile | row = tile.row - 1 }
      , { tile | col = tile.col - 1 }
      , { tile | col = tile.col + 1 }
      , { tile | row = tile.row + 1 }
      ]

    hasTilesInCommon : List Tile -> List Tile -> Bool
    hasTilesInCommon a b =
      List.any (\tile -> List.any ((==) tile) b) a
  in
    case tiles of
      [] ->
        False

      (first :: rest) ->
        hasTilesInCommon (possibleMoves first) rest || hasTileMatches rest

type Direction
  = Up
  | Down
  | Left
  | Right

type alias Movement =
  { tiles : List Tile
  , score : Int
  , moved : Bool
  }

-- Takes a list of tiles, in any order, and moves them in the given direction.
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
-- { tiles = [ { row = 0, col = 0, value = 2 }, { row = 0, col = 1, value = 2 }
--           , { row = 0, col = 2, value = 4 }, { row = 0, col = 3, value = 4 }
--           , { row = 1, col = 1, value = 16 }, { row = 1, col = 3, value = 32 }
--           ]
-- , score = 0
-- , moved = True
-- }
-- tilesU = .tiles (move Up tiles)
--
-- move Right tilesU
-- =>
-- { tiles = [ { row = 0, col = 2, value = 4 }, { row = 0, col = 3, value = 8 }
--           , { row = 1, col = 2, value = 16 }, { row = 1, col = 3, value = 32 }
--           ]
-- , score = 12
-- , moved = True
-- }
-- tilesUR = .tiles (move Right tilesU)
--
-- move Left tiles
-- =>
-- { tiles = [ { row = 0, col = 0, value = 2 }, { row = 0, col = 1, value = 4 }
--           , { row = 2, col = 0, value = 2 }, { row = 2, col = 1, value = 16 }
--           , { row = 3, col = 0, value = 4 }, { row = 3, col = 1, value = 32 }
--           ]
-- , score = 0
-- , moved = True
-- }
-- tilesL = .tiles (move Left tiles)
--
-- move Down tilesL
-- =>
-- { tiles = [ { row = 1, col = 1, value = 4 },
--           , { row = 2, col = 0, value = 4 }, { row = 2, col = 1, value = 16 }
--           , { row = 3, col = 0, value = 4 }, { row = 3, col = 1, value = 32 }
--           ]
-- , score = 4
-- , moved = True
-- }
-- tilesLD = .tiles (move Down tilesL)
--
-- N.B. The order of tiles, tilesU, tilesUR, tilesL and tilesLD doesn't matter.
move : Direction -> List Tile -> Movement
move dir tiles =
  let
    combine : List Movement -> Movement
    combine =
      List.foldl
        (\next all ->
          { tiles = next.tiles ++ all.tiles
          , score = next.score + all.score
          , moved = next.moved || all.moved
          }
        )
        { tiles = [], score = 0, moved = False }
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

-- Generate tile values such that a 2 is generated with probability 90% and a
-- 4 is generated with probability 10%.
valueGen : Generator Int
valueGen =
  Random.map (\x -> if x < 0.9 then 2 else 4) (Random.float 0 1)

-- Generate one tile in an available position.
--
-- N.B. It assumes that at least one position is available.
oneTileGen : List Tile -> Generator Tile
oneTileGen tiles =
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
-- N.B.: It assumes that at least two positions are available.
twoTileGen : List Tile -> Generator (Tile, Tile)
twoTileGen tiles =
  oneTileGen tiles
    |> Random.andThen
      (\tile ->
        oneTileGen (tile :: tiles)
          |> Random.map ((,) tile)
      )

-- Generate a grid with two tiles in any of the available positions.
startGridGen : Generator (List Tile)
startGridGen =
  twoTileGen []
    |> Random.map (\(tile1, tile2) -> [ tile1, tile2 ])

-- Generate a grid with one extra tile added in one of the available positions.
nextGridGen : List Tile -> Generator (List Tile)
nextGridGen tiles =
  oneTileGen tiles
    |> Random.map (\tile -> tile :: tiles)

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
-- { tiles = [ { row = 1, col = 0, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- , score = 0
-- , moved = True
-- }
--
-- moveUp [ { row = 1, col = 0, value = 2 }, { row = 3, col = 0, value = 2 } ]
-- =>
-- { tiles = [ { row = 0, col = 0, value = 4 } ]
-- , score = 4
-- , moved = True
-- )
--
-- moveUp [ { row = 0, col = 0, value = 4 } ]
-- =>
-- { tiles = [ { row = 0, col = 0, value = 4 } ]
-- , score = 0
-- , moved = False
-- }
moveUp : List Tile -> Movement
moveUp tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> Bool -> List Tile -> Movement
    move farthest prev accum score moved tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              { tiles = accum, score = score, moved = moved }

            Just prev ->
              { tiles = prev :: accum, score = score, moved = moved }

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
-- { tiles = [ { row = 2, col = 0, value = 2 }, { row = 3, col = 0, value = 4 } ]
-- , score = 0
-- , moved = True
-- }
--
-- moveDown [ { row = 2, col = 0, value = 2 }, { row = 0, col = 0, value = 2 } ]
-- =>
-- { tiles = [ { row = 3, col = 0, value = 4 } ]
-- , score = 4
-- , moved = True
-- }
--
-- moveDown [ { row = 3, col = 0, value = 4 } ]
-- =>
-- { tiles = [ { row = 3, col = 0, value = 4 } ]
-- , score = 0
-- , moved = False
-- }
moveDown : List Tile -> Movement
moveDown tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> Bool -> List Tile -> Movement
    move farthest prev accum score moved tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              { tiles = accum, score = score, moved = moved }

            Just prev ->
              { tiles = prev :: accum, score = score, moved = moved }

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
    move (cellCount - 1) Nothing [] 0 False tiles

-- Takes a list of tiles, that are in the same row, ordered from left to right
-- and moves them to the farthest left position they can move. If adjacent tiles
-- have the same value then they are merged (at most once).
--
-- Examples:
--
-- moveLeft [ { row = 0, col = 1, value = 2 }, { row = 0, col = 3, value = 4 } ]
-- =>
-- { tiles = [ { row = 0, col = 1, value = 4 }, { row = 0, col = 0, value = 2 } ]
-- , score = 0
-- , moved = True
-- }
--
-- moveLeft [ { row = 0, col = 1, value = 2 }, { row = 0, col = 3, value = 2 } ]
-- =>
-- { tiles = [ { row = 0, col = 0, value = 4 } ]
-- , score = 4
-- , moved = True
-- }
--
-- moveLeft [ { row = 0, col = 0, value = 4 } ]
-- =>
-- { tiles = [ { row = 0, col = 0, value = 4 } ]
-- , score = 0
-- , moved = False
-- }
moveLeft : List Tile -> Movement
moveLeft tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> Bool -> List Tile -> Movement
    move farthest prev accum score moved tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              { tiles = accum, score = score, moved = moved }

            Just prev ->
              { tiles = prev :: accum, score = score, moved = moved }

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
-- { tiles = [ { row = 0, col = 2, value = 2 }, { row = 0, col = 3, value = 4 } ]
-- , score = 0
-- , moved = True
-- }
--
-- moveRight [ { row = 0, col = 2, value = 2 }, { row = 0, col = 0, value = 2 } ]
-- =>
-- { tiles = [ { row = 0, col = 3, value = 4 } ]
-- , score = 4
-- , moved = True
-- }
--
-- moveRight [ { row = 0, col = 3, value = 4 } ]
-- =>
-- { tiles = [ { row = 0, col = 3, value = 4 } ]
-- , score = 0
-- , moved = False
-- }
moveRight : List Tile -> Movement
moveRight tiles =
  let
    move : Int -> Maybe Tile -> List Tile -> Int -> Bool -> List Tile -> Movement
    move farthest prev accum score moved tiles =
      case tiles of
        [] ->
          case prev of
            Nothing ->
              { tiles = accum, score = score, moved = moved }

            Just prev ->
              { tiles = prev :: accum, score = score, moved = moved }

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
    move (cellCount - 1) Nothing [] 0 False tiles

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
