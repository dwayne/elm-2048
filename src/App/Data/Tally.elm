module App.Data.Tally exposing
  ( Tally, Reckoning, zero
  , resetCurrent, addPoints
  , toReckoning
  )


import App.Data.Points as Points exposing (Points)


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
