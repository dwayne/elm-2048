module App.Data.Points exposing
  ( Points, zero, fromValue
  , add, max
  , toString
  )


import App.Data.Tile.Value as Value exposing (Value)


type Points
  = Points Int


zero : Points
zero =
  Points 0


fromValue : Value -> Points
fromValue =
  Points << Value.toInt


add : Points -> Points -> Points
add (Points a) (Points b) =
  Points <| a + b


max : Points -> Points -> Points
max (Points a as p1) (Points b as p2) =
  if a >= b then p1 else p2


toString : Points -> String
toString (Points n) =
  String.fromInt n
