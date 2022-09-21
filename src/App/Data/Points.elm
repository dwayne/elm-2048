module App.Data.Points exposing
  ( Points, zero, fromInt
  , add, max
  , toString
  )


type Points
  = Points Int


zero : Points
zero =
  Points 0


fromInt : Int -> Points
fromInt n =
  if n > 0 then
    Points n
  else
    Points 0


add : Points -> Points -> Points
add (Points a) (Points b) =
  Points <| a + b


max : Points -> Points -> Points
max (Points a as p1) (Points b as p2) =
  if a >= b then p1 else p2


toString : Points -> String
toString (Points n) =
  String.fromInt n
