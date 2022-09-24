module App.Data.Tile.Value exposing
  ( Value, two, four
  , isEqual
  , generator
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


isEqual : Value -> Value -> Bool
isEqual (Value a) (Value b) =
  a == b


generator : Random.Generator Value
generator =
  Random.weighted (9, two) [(1, four)]


double : Value -> Value
double (Value n) =
  Value <| 2 * n


toInt : Value -> Int
toInt (Value n) =
  n


toString : Value -> String
toString (Value n) =
  String.fromInt n
