module App.Data.Tile exposing
  ( Tile, new
  , getPosition

  , Info
  , toInfo
  )


import App.Data.Tile.Position as Position exposing (Position)
import App.Data.Tile.Value as Value exposing (Value)


type Tile
  = New
      { id : Int
      , value : Value
      , position : Position
      }


new : Int -> Value -> Position -> Tile
new id value position =
  New
    { id = id
    , value = value
    , position = position
    }


getPosition : Tile -> Position
getPosition tile =
  case tile of
    New { position } ->
      position


type alias Info =
  { status : String
  , id : Int
  , value : Value
  , position : Position
  }


toInfo : Tile -> Info
toInfo tile =
  case tile of
    New { id, value, position } ->
      { status = "new"
      , id = id
      , value = value
      , position = position
      }
