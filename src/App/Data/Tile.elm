module App.Data.Tile exposing
  ( Tile, State, Action(..)
  , new, composite, merged, old
  , getId, getValue, getPosition
  , age
  , toMerged

  , Info
  , toInfo
  )


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


getId : Tile -> Int
getId tile =
  case tile of
    New { id } ->
      id

    Composite { id } ->
      id

    Merged { id } _ ->
      id

    Old { id } _ ->
      id


getValue : Tile -> Value
getValue tile =
  case tile of
    New { value } ->
      value

    Composite { value } ->
      value

    Merged { value } _ ->
      value

    Old { value } _ ->
      value


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


age : Tile -> Maybe State
age tile =
  case tile of
    New state ->
      Just state

    Composite state ->
      Just state

    Merged _ _ ->
      Nothing

    Old state _ ->
      Just state


toMerged : Tile -> Tile
toMerged tile =
  case tile of
    Old state action ->
      Merged state action

    _ ->
      tile


type alias Info =
  { kind : String
  , state : State
  , action : Action
  }


toInfo : Tile -> Info
toInfo tile =
  case tile of
    New state ->
      { kind = "new"
      , state = state
      , action = Stay
      }

    Composite state ->
      { kind = "composite"
      , state = state
      , action = Stay
      }

    Merged state action ->
      { kind = "merged"
      , state = state
      , action = action
      }

    Old state action ->
      { kind = "old"
      , state = state
      , action = action
      }
