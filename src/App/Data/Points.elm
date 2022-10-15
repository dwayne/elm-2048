module App.Data.Points exposing
    ( Points
    , add
    , decoder
    , encode
    , fromValue
    , isZero
    , max
    , toString
    , zero
    )

import App.Data.Tile.Value as Value exposing (Value)
import Json.Decode as JD
import Json.Encode as JE


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
max ((Points a) as p1) ((Points b) as p2) =
    if a >= b then
        p1

    else
        p2


toString : Points -> String
toString (Points n) =
    String.fromInt n


encode : Points -> JE.Value
encode (Points n) =
    JE.int n


decoder : JD.Decoder Points
decoder =
    JD.map Points nonNegativeEvenDecoder


nonNegativeEvenDecoder : JD.Decoder Int
nonNegativeEvenDecoder =
    JD.int
        |> JD.andThen
            (\n ->
                if n >= 0 && modBy 2 n == 0 then
                    JD.succeed n

                else
                    JD.fail <| "expected a non-negative even integer: " ++ String.fromInt n
            )
