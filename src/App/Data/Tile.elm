module App.Data.Tile exposing
  ( Tile, new, composite, merged, old
  , getPosition
  , age
  , Info, toInfo
  , toPoints
  )


import App.Data.Points as Points exposing (Points)
import App.Data.Tile.Position as Position exposing (Position)
import App.Data.Tile.Value as Value exposing (Value)


type Tile
  = New State
  | Composite State
  | Merged State Action
  | Old State Action


type alias State =
  { id : Int
  , value : Value
  , position : Position
  }


type Action
  = Stay
  | MoveFrom Position


new : Int -> Value -> Position -> Tile
new id value position =
  New <| State id value position


composite : Int -> Value -> Position -> Tile
composite id value position =
  Composite <| State id value position


merged : Int -> Value -> Position -> Position -> Tile
merged id value from to =
  Merged (State id value to) <|
    if from == to then
      Stay
    else
      MoveFrom from


old : Int -> Value -> Position -> Position -> Tile
old id value from to =
  Old (State id value to) <|
    if from == to then
      Stay
    else
      MoveFrom from


getPosition : Tile -> Position
getPosition tile =
  case tile of
    New { position } ->
      position

    Composite { position } ->
      position

    Merged { position } _ ->
      position

    Old { position } _ ->
      position


age : Tile -> Maybe Tile
age tile =
  case tile of
    New state ->
      Just <| Old state Stay

    Composite state ->
      Just <| Old state Stay

    Merged _ _ ->
      Nothing

    Old state _ ->
      Just <| Old state Stay


type alias Info =
  { kind : String
  , id : Int
  , value : Value
  , from : Position
  , to : Position
  }


toInfo : Tile -> Info
toInfo tile =
  case tile of
    New { id, value, position } ->
      { kind = "new"
      , id = id
      , value = value
      , from = position
      , to = position
      }

    Composite { id, value, position } ->
      { kind = "composite"
      , id = id
      , value = value
      , from = position
      , to = position
      }

    Merged { id, value, position } action ->
      { kind = "merged"
      , id = id
      , value = value
      , from =
          case action of
            Stay ->
              position

            MoveFrom from ->
              from
      , to = position
      }

    Old { id, value, position } action ->
      { kind = "old"
      , id = id
      , value = value
      , from =
          case action of
            Stay ->
              position

            MoveFrom from ->
              from
      , to = position
      }


toPoints : Tile -> Points
toPoints tile =
  case tile of
    Composite { value } ->
      Points.fromValue value

    _ ->
      Points.zero
