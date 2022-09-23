module App.Data.Grid exposing
  ( Grid, empty
  , generator

  , TransparentTile
  , toTransparentTiles
  )


import App.Data.Tile.Position as Position exposing (Position)
import App.Data.Tile.Value as Value exposing (Value)
import App.Lib.List as List
import Random


type Grid
  = Grid (List Tile)

type Tile
  = New Value Position
  | Composite Value Position
  | Merged Value Translation
  | Old Value Translation

type alias Translation =
  { from : Position
  , to : Position
  }


empty : Grid
empty =
  Grid []


generator : Grid -> Random.Generator (Maybe Grid)
generator grid =
  getUnavailablePositions grid
    |> tileGenerator
    |> Random.andThen
        (\newTiles ->
          if List.isEmpty newTiles then
            Random.constant Nothing

          else
            Random.constant <| Just <| addTiles newTiles grid
        )


tileGenerator : List Position -> Random.Generator (List Tile)
tileGenerator unavailablePositions =
  Position.generator unavailablePositions
    |> Random.andThen
        (\positions ->
          if List.isEmpty positions then
            Random.constant []

          else
            Random.list (List.length positions) Value.generator
              |> Random.andThen
                  (\values ->
                    List.zip values positions
                      |> List.map (\(value, position) -> New value position)
                      |> Random.constant
                  )
        )


getUnavailablePositions : Grid -> List Position
getUnavailablePositions (Grid tiles) =
  List.map getPosition tiles


getPosition : Tile -> Position
getPosition tile =
  case tile of
    New _ position ->
      position

    Composite _ position ->
      position

    Merged _ { to } ->
      to

    Old _ { to } ->
      to


addTiles : List Tile -> Grid -> Grid
addTiles newTiles (Grid tiles) =
  Grid <| tiles ++ newTiles


type alias TransparentTile =
  { kind : String
  , value : Value
  , position : Position
  }


toTransparentTiles : Grid -> List TransparentTile
toTransparentTiles (Grid tiles) =
  List.map toTransparentTile tiles


toTransparentTile : Tile -> TransparentTile
toTransparentTile tile =
  case tile of
    New value position ->
      { kind = "new"
      , value = value
      , position = position
      }

    Composite value position ->
      { kind = "composite"
      , value = value
      , position = position
      }

    Merged value { to } ->
      { kind = "merged"
      , value = value
      , position = to
      }

    Old value { to } ->
      { kind = "old"
      , value = value
      , position = to
      }
