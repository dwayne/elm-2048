module App.Data.Tile.Value exposing
  ( Value, two, four
  , twoOrFour
  , isEqual
  , double
  , toInt, toString
  )


import Random


type Value
  = Value Int


two : Value
two =
  Value 2


four : Value
four =
  Value 4


twoOrFour : Random.Generator Value
twoOrFour =
  Random.weighted (9, two) [(1, four)]


isEqual : Value -> Value -> Bool
isEqual (Value v1) (Value v2) =
  v1 == v2


double : Value -> Value
double (Value v) =
  Value <| 2 * v


toInt : Value -> Int
toInt (Value v) =
  v


toString : Value -> String
toString (Value v) =
  String.fromInt v
