module App.Data.Tally exposing
  ( Tally, Reckoning, zero
  , resetCurrent, addPoints
  , toReckoning
  , encode, decoder
  )


import App.Data.Points as Points exposing (Points)
import Json.Decode as JD
import Json.Encode as JE


type Tally
  = Tally Reckoning


type alias Reckoning =
  { current : Points
  , best : Points
  }


zero : Tally
zero =
  Tally
    { current = Points.zero
    , best = Points.zero
    }


resetCurrent : Tally -> Tally
resetCurrent (Tally reckoning) =
  Tally { reckoning | current = Points.zero }


addPoints : Points -> Tally -> Tally
addPoints points (Tally { current, best }) =
  let
    newCurrent =
      Points.add points current
  in
  Tally
    { current = newCurrent
    , best = Points.max newCurrent best
    }


toReckoning : Tally -> Reckoning
toReckoning (Tally reckoning) =
  reckoning


encode : Tally -> JE.Value
encode (Tally { current, best }) =
  JE.object
    [ ( "current", Points.encode current )
    , ( "best", Points.encode best )
    ]


decoder : JD.Decoder Tally
decoder =
  JD.map Tally reckoningDecoder


reckoningDecoder : JD.Decoder Reckoning
reckoningDecoder =
  JD.map2 Reckoning
    (JD.field "current" Points.decoder)
    (JD.field "best" Points.decoder)
