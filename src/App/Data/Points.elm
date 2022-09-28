module App.Data.Points exposing
  ( Points, zero, fromValue
  , isZero
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


isZero : Points -> Bool
isZero (Points n) =
  n == 0


add : Points -> Points -> Points
add (Points a) (Points b) =
  Points <| a + b


max : Points -> Points -> Points
max (Points a as p1) (Points b as p2) =
  if a >= b then p1 else p2


toString : Points -> String
toString (Points n) =
  String.fromInt n
