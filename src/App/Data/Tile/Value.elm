module App.Data.Tile.Value exposing
  ( Value, two, four
  , twoOrFour
  , isEqual, is2048
  , double
  , toString, toInt
  , encode, decoder
  )


import Json.Decode as JD
import Json.Encode as JE
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


is2048 : Value -> Bool
is2048 (Value v) =
  v == 2048


double : Value -> Value
double (Value v) =
  Value <| 2 * v


toString : Value -> String
toString (Value v) =
  String.fromInt v


toInt : Value -> Int
toInt (Value v) =
  v


encode : Value -> JE.Value
encode (Value v) =
  JE.int v


decoder : JD.Decoder Value
decoder =
  JD.map Value positivePowerOf2Decoder


positivePowerOf2Decoder : JD.Decoder Int
positivePowerOf2Decoder =
  JD.int
    |> JD.andThen
        (\n ->
          if isPositivePowerOf2 n then
            JD.succeed n

          else
            JD.fail <| "expected a positive power of 2: " ++ String.fromInt n
        )


isPositivePowerOf2 : Int -> Bool
isPositivePowerOf2 n =
  if n == 2 then
    True
  else if n > 2 && modBy 2 n == 0 then
    isPositivePowerOf2 <| n // 2
  else
    False
