module App.Data.Tile exposing
  ( Tile, State, new, composite, merged, old
  , getPosition
  , comparator
  , age

  , Info
  , toInfo
  )


import App.Data.Tile.Position as Position exposing (Position)
import App.Data.Tile.Value as Value exposing (Value)


type Tile
  = New State
  | Composite State
  | Merged State
  | Old State


type alias State =
  { id : Int
  , value : Value
  , position : Position
  }


new : Int -> Value -> Position -> Tile
new id value position =
  New <| State id value position


composite : Int -> Value -> Position -> Tile
composite id value position =
  Composite <| State id value position


merged : Int -> Value -> Position -> Tile
merged id value position =
  Merged <| State id value position


old : Int -> Value -> Position -> Tile
old id value position =
  Old <| State id value position


getId : Tile -> Int
getId tile =
  case tile of
    New { id } ->
      id

    Composite { id } ->
      id

    Merged { id } ->
      id

    Old { id } ->
      id


getPosition : Tile -> Position
getPosition tile =
  case tile of
    New { position } ->
      position

    Composite { position } ->
      position

    Merged { position } ->
      position

    Old { position } ->
      position


comparator : Tile -> Tile -> Order
comparator tile1 tile2 =
  case (tile1, tile2) of
    (Old state1, Old state2) ->
      Position.comparator state1.position state2.position

    (Old state1, Merged state2) ->
      Position.comparator state1.position state2.position

    (Merged state1, Old state2) ->
      Position.comparator state1.position state2.position

    (Merged state1, Merged state2) ->
      Position.comparator state1.position state2.position

    (Old _, _) ->
      LT

    (Merged _, _) ->
      LT

    (_, Old _) ->
      GT

    (_, Merged _) ->
      GT

    (Composite state1, Composite state2) ->
      Position.comparator state1.position state2.position

    (Composite _, _) ->
      LT

    (_, Composite _) ->
      GT

    (New state1, New state2) ->
      Position.comparator state1.position state2.position


age : Tile -> Maybe State
age tile =
  case tile of
    New state ->
      Just state

    Composite state ->
      Just state

    Merged _ ->
      Nothing

    Old state ->
      Just state


type alias Info =
  { kind : String
  , state : State
  }


toInfo : Tile -> Info
toInfo tile =
  case tile of
    New state ->
      { kind = "new"
      , state = state
      }

    Composite state ->
      { kind = "composite"
      , state = state
      }

    Merged state ->
      { kind = "merged"
      , state = state
      }

    Old state ->
      { kind = "old"
      , state = state
      }
