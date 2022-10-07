module App.Data.Grid exposing
  ( Grid, empty, reset
  , has2048, hasMoves
  , insertAtMost2Tiles
  , Direction(..), move
  , toPoints, toTiles
  )


import App.Data.Points as Points exposing (Points)
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


has2048 : Grid -> Bool
has2048 (Grid { tiles }) =
  List.any Tile.is2048 tiles


hasMoves : Grid -> Bool
hasMoves (Grid { tiles } as grid) =
  let
    unavailablePositions =
      List.map Tile.getPosition tiles

    hasAvailablePositions =
      not <| List.isEmpty <| Position.availablePositions unavailablePositions

    canMoveRight =
      \_ -> move Right grid /= Nothing

    canMoveUp =
      \_ -> move Up grid /= Nothing
  in
  hasAvailablePositions || canMoveRight () || canMoveUp ()


insertAtMost2Tiles : Grid -> Random.Generator (Maybe Grid)
insertAtMost2Tiles (Grid { currentId, tiles }) =
  let
    unavailablePositions =
      List.map Tile.getPosition tiles
  in
  valueAndPositionGenerator unavailablePositions
    |> Random.andThen
        (\valueAndPositions ->
          if List.isEmpty valueAndPositions then
            Random.constant Nothing

          else
            let
              (newCurrentId, newTiles) =
                addTiles valueAndPositions currentId tiles
            in
            Grid
              { currentId = newCurrentId
              , tiles = newTiles
              }
              |> Just
              |> Random.constant
        )


valueAndPositionGenerator : List Position -> Random.Generator (List (Value, Position))
valueAndPositionGenerator unavailablePositions =
  Position.selectAtMost2 unavailablePositions
    |> Random.andThen
        (\positions ->
          if List.isEmpty positions then
            Random.constant []

          else
            Random.list (List.length positions) Value.twoOrFour
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
        (Tile.new currentId value position :: tiles)


type Direction
  = Right
  | Left
  | Down
  | Up


type alias MovementConfig =
  { toStartingPosition : Position -> Position
  , updateLastPosition : Position -> Position
  , isReadyToVacate : Position -> Position -> Bool
  }


type alias MovementState =
  { currentId : Int
  , lastPosition : Position
  , infoInCell : Maybe Tile.Info
  , newTiles : List Tile
  , atLeastOneTileMoved : Bool
  }


move : Direction -> Grid -> Maybe Grid
move direction (Grid { currentId, tiles })=
  let
    state =
      case direction of
        Right ->
          tiles
            |> age
            |> List.sortWith compareRight
            |> moveHelper
                { toStartingPosition = \(r, _) -> (r, 4)
                , updateLastPosition = \(r, c) -> (r, c - 1)
                , isReadyToVacate =
                    \(tileRow, _) (lastRow, _) -> tileRow > lastRow
                }
                { currentId = currentId
                , lastPosition = (1, 4)
                , infoInCell = Nothing
                , newTiles = []
                , atLeastOneTileMoved = False
                }

        Left ->
          tiles
            |> age
            |> List.sortWith compareLeft
            |> moveHelper
                { toStartingPosition = \(r, _) -> (r, 1)
                , updateLastPosition = \(r, c) -> (r, c + 1)
                , isReadyToVacate =
                    \(tileRow, _) (lastRow, _) -> tileRow > lastRow
                }
                { currentId = currentId
                , lastPosition = (1, 1)
                , infoInCell = Nothing
                , newTiles = []
                , atLeastOneTileMoved = False
                }

        Down ->
          tiles
            |> age
            |> List.sortWith compareDown
            |> moveHelper
                { toStartingPosition = \(_, c) -> (4, c)
                , updateLastPosition = \(r, c) -> (r - 1, c)
                , isReadyToVacate =
                    \(_, tileCol) (_, lastCol) -> tileCol > lastCol
                }
                { currentId = currentId
                , lastPosition = (4, 1)
                , infoInCell = Nothing
                , newTiles = []
                , atLeastOneTileMoved = False
                }

        Up ->
          tiles
            |> age
            |> List.sortWith compareUp
            |> moveHelper
                { toStartingPosition = \(_, c) -> (1, c)
                , updateLastPosition = \(r, c) -> (r + 1, c)
                , isReadyToVacate =
                    \(_, tileCol) (_, lastCol) -> tileCol > lastCol
                }
                { currentId = currentId
                , lastPosition = (1, 1)
                , infoInCell = Nothing
                , newTiles = []
                , atLeastOneTileMoved = False
                }
  in
  if state.atLeastOneTileMoved then
    Just <|
      Grid
        { currentId = state.currentId
        , tiles = state.newTiles
        }
  else
    Nothing


moveHelper : MovementConfig -> MovementState -> List Tile -> MovementState
moveHelper config state tiles =
  case tiles of
    [] ->
      case state.infoInCell of
        Nothing ->
          state

        Just { id, value, from, to } ->
          { state
          | lastPosition = to
          , infoInCell = Nothing
          , newTiles = Tile.old id value from to :: state.newTiles
          }

    tile :: restTiles ->
      let
        currInfo =
          Tile.toInfo tile

        state1 =
          if config.isReadyToVacate currInfo.to state.lastPosition then
            { state
            | lastPosition = config.toStartingPosition currInfo.to
            , infoInCell = Nothing
            , newTiles =
                case state.infoInCell of
                  Nothing ->
                    state.newTiles

                  Just { id, value, from, to } ->
                    Tile.old id value from to :: state.newTiles
            }
          else
            state

        state2 =
          case state1.infoInCell of
            Nothing ->
              { state1
              | infoInCell =
                  Just
                    { currInfo
                    | from = currInfo.to
                    , to = state1.lastPosition
                    }
              , atLeastOneTileMoved =
                  state1.atLeastOneTileMoved ||
                    (currInfo.to /= state1.lastPosition)
              }

            Just { id, value, from, to } ->
              let
                lastPosition =
                  config.updateLastPosition state1.lastPosition
              in
              if Value.isEqual currInfo.value value then
                { state1
                | currentId = state1.currentId + 1
                , lastPosition = lastPosition
                , infoInCell = Nothing
                , newTiles =
                    [ Tile.merged currInfo.id currInfo.value currInfo.to to
                    , Tile.merged id value from to
                    , Tile.composite
                        state1.currentId
                        (Value.double value)
                        to
                    ] ++ state1.newTiles
                , atLeastOneTileMoved = True
                }
              else
                { state1
                | lastPosition = lastPosition
                , infoInCell =
                    Just { currInfo | from = currInfo.to, to = lastPosition }
                , newTiles = Tile.old id value from to :: state1.newTiles
                , atLeastOneTileMoved =
                    state1.atLeastOneTileMoved || (currInfo.to /= lastPosition)
                }
      in
      moveHelper config state2 restTiles


age : List Tile -> List Tile
age =
  List.filterMap Tile.age


compareRight : Tile -> Tile -> Order
compareRight tile1 tile2 =
  let
    ((r1, c1), (r2, c2)) =
      ( Tile.getPosition tile1
      , Tile.getPosition tile2
      )
  in
  List.compareBy [(r1, r2), (c2, c1)]


compareLeft : Tile -> Tile -> Order
compareLeft tile1 tile2 =
  let
    ((r1, c1), (r2, c2)) =
      ( Tile.getPosition tile1
      , Tile.getPosition tile2
      )
  in
  List.compareBy [(r1, r2), (c1, c2)]


compareDown : Tile -> Tile -> Order
compareDown tile1 tile2 =
  let
    ((r1, c1), (r2, c2)) =
      ( Tile.getPosition tile1
      , Tile.getPosition tile2
      )
  in
  List.compareBy [(c1, c2), (r2, r1)]


compareUp : Tile -> Tile -> Order
compareUp tile1 tile2 =
  let
    ((r1, c1), (r2, c2)) =
      ( Tile.getPosition tile1
      , Tile.getPosition tile2
      )
  in
  List.compareBy [(c1, c2), (r1, r2)]


toPoints : Grid -> Points
toPoints (Grid { tiles }) =
  tiles
    |> List.map Tile.toPoints
    |> List.foldr Points.add Points.zero


toTiles : Grid -> List Tile
toTiles (Grid { tiles }) =
  tiles
